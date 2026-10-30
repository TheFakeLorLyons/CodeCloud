(ns codecloud.languages.c-family-test
  (:require [clojure.test :refer [deftest is testing]]
            [codecloud.fixtures :as fixtures]
            [codecloud.languages :as languages]
            [codecloud.languages.c-family :as c-family]
            [codecloud.languages.lexer :as lexer]
            [codecloud.languages.lexicons :as lexicons]
            [codecloud.languages.manifests :as manifests]
            [codecloud.languages.scanner :as scanner]
            [codecloud.model.facts :as facts]
            [codecloud.model.snapshot :as snapshot]
            [codecloud.model.source :as source]))

(defn- tokens-of [language-id lines]
  (scanner/significant (scanner/token-stream language-id lines)))

(deftest finds-includes
  (is (= [["Board.hpp" false 1] ["vector" true 2] ["eeg/next.h" true 3]]
         (mapv (juxt :specifier :system? :line)
               (c-family/includes (tokens-of :cpp ["#include \"Board.hpp\""
                                                   "#include <vector>"
                                                   "#  include_next <eeg/next.h>"
                                                   "#include CONFIG_HEADER"
                                                   "// #include \"commented.h\""
                                                   "const char *text = \"#include <fake.h>\";"]))))))

(deftest reads-a-header-as-c-or-c++-by-its-code
  (is (= :c (c-family/file-language :c "include/filter.h" ["#ifdef __cplusplus" "extern \"C\" {" "#endif"
                                                           "double filter_sample(double sample);"
                                                           "#ifdef __cplusplus" "}" "#endif"])))
  (is (= :cpp (c-family/file-language :c "include/ring.h" ["namespace eeg {" "}"])))
  (is (= :cpp (c-family/file-language :c "include/board.h" ["class Board {" " public:" "  int read();" "};"])))
  (is (= :cpp (c-family/file-language :c "include/twice.h" ["inline int twice(int x) { return std::abs(x) * 2; }"])))
  (is (= :c (c-family/file-language :c "src/odd.c" ["namespace x {}"])) "a .c file stays C")
  (is (= :cpp (c-family/file-language :cpp "src/main.cpp" ["int main() {}"]))))

(defn- sample-facts []
  (let [path "test-resources/c-family/sample.cpp"]
    (languages/analyze-file path (fixtures/read-text path) {:parse-tree (fixtures/recorded-parse-tree path)})))

(deftest names-c++-definitions-as-written
  (let [file-facts (sample-facts)]
    (is (empty? (facts/problems file-facts)))
    (is (= :cpp (:language file-facts)))
    (is (= [[3 "SQUARE" :macro] [4 "LIMIT" :value] [7 "Ring" :type] [10 "Ring::Ring" :function]
            [12 "Ring::operator[]" :function] [13 "Ring::operator bool" :function]
            [14 "Ring::operator()" :function] [15 "Ring::Node" :type] [15 "Ring::Node::value" :function]
            [20 "Ring::front" :function] [23 "get_handler" :function] [24 "name_of" :function]
            [25 "cache" :function] [26 "Point" :type] [27 "Color" :type] [28 "Samples" :type]
            [29 "kRate" :value] [30 "helper" :function] [32 "hidden" :function] [35 "c_api" :function]
            [38 "debug_dump" :function] [40 "debug_dump" :function] [42 "TEST(RingTest, Grows)" :test]]
           (mapv (juxt :line :name :kind) (:definitions file-facts))))
    (testing "static functions and those in an unnamed namespace are private"
      (is (= #{"helper" "hidden"} (set (map :name (filter :private? (:definitions file-facts)))))))
    (testing "without a syntax tree a file still has its includes"
      (let [plain-facts (languages/analyze-file "test-resources/c-family/sample.cpp"
                                                (fixtures/read-text "test-resources/c-family/sample.cpp") {})]
        (is (empty? (:definitions plain-facts)))
        (is (= ["vector" "eeg/filter.hpp"] (mapv :specifier (:imports plain-facts))))))))

(defn- units-of
  "Path to unit for C++ files at `paths`, once paired."
  [paths]
  (into {}
        (map (juxt :path :unit-id))
        (c-family/paired-units (map (fn [path] {:path path :unit-id path :language :cpp}) paths))))

(deftest pairs-headers-with-their-sources
  (testing "in the same folder"
    (is (= {"eeg/Board.hpp" "eeg/Board.cpp" "eeg/Board.cpp" "eeg/Board.cpp"}
           (units-of ["eeg/Board.hpp" "eeg/Board.cpp"]))))
  (testing "across folders, when one source and one header are left with the stem"
    (is (= "src/filter.c" (get (units-of ["include/filter.h" "src/filter.c"]) "include/filter.h"))))
  (testing "never when two sources could own the header"
    (is (= "include/util.h" (get (units-of ["include/util.h" "a/util.c" "b/util.c"]) "include/util.h"))))
  (testing "a header paired beside its source leaves a lone header elsewhere alone"
    (is (= {"src/a.c" "src/a.c" "src/a.h" "src/a.c" "include/a.h" "include/a.h"}
           (units-of ["src/a.c" "src/a.h" "include/a.h"]))))
  (testing "an inline file follows the header beside it"
    (is (= "lib/ring.hpp" (get (units-of ["lib/ring.hpp" "lib/ring.tpp"]) "lib/ring.tpp")))
    (is (= "src/x.cpp" (get (units-of ["include/x.h" "include/x.inl" "src/x.cpp"]) "include/x.inl")))))

(defn- built-snapshot
  "A snapshot of `files`, path to text, with build files read as
  manifests."
  [files include-paths]
  (snapshot/build-snapshot (for [[path text] files
                                 :let [file-name (source/file-name path)]]
                             (if (contains? manifests/file-names file-name)
                               (manifests/read-manifest path file-name text)
                               (languages/analyze-file path text {})))
                           {:id :working-tree :label "Working tree" :time 0 :include-paths include-paths}))

(deftest resolves-includes
  (let [built (built-snapshot {"app/main.c" (str "#include \"util.h\"\n#include <config/limits.h>\n#include <stdio.h>\n"
                                                 "int main(void) { return twice(LIMIT); }\n")
                               "lib/include/util.h" "int twice(int x);\n"
                               "lib/util.c" "#include \"util.h\"\nint twice(int x) { return 2 * x; }\n"
                               "third/config/limits.h" "#define LIMIT 4\n"
                               "lib/CMakeLists.txt" "include_directories(include)\n"}
                              ["third"])]
    (is (= #{["app/main.c" "lib/util.c" :include] ["app/main.c" "third/config/limits.h" :include]}
           (set (map (juxt :from :to :kind) (:edges built)))))
    (is (= ["stdio.h"] (get-in built [:namespaces "app/main.c" :external-requires])))
    (is (= ["lib/include/util.h" "lib/util.c"] (mapv :path (get-in built [:namespaces "lib/util.c" :files])))))
  (testing "a header found only by its ending goes to the nearest one, or to none on a tie"
    (let [files {"a/x/common.h" "" "b/x/common.h" ""}
          from-a (built-snapshot (assoc files "a/src/main.c" "#include \"x/common.h\"\n") [])
          from-c (built-snapshot (assoc files "c/main.c" "#include \"x/common.h\"\n") [])]
      (is (= [["a/src/main.c" "a/x/common.h"]] (mapv (juxt :from :to) (:edges from-a))))
      (is (empty? (:edges from-c)))
      (is (= ["x/common.h"] (get-in from-c [:namespaces "c/main.c" :external-requires]))))))

(deftest reads-include-folders-from-build-files
  (let [folders (fn [file-name text] (get-in (manifests/read-manifest (str "native/" file-name) file-name text)
                                             [:manifest :include-folders]))]
    (is (= [{:folder "include" :base :manifest}
            {:folder "third_party/json/single_include" :base :root}
            {:folder "/opt/sdk/include" :base :absolute}]
           (folders "CMakeLists.txt"
                    (str "include_directories(include)\n"
                         "target_include_directories(app PUBLIC"
                         " $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/third_party/json/single_include>"
                         " $<INSTALL_INTERFACE:include> ${OTHER}/x)\n"
                         "include_directories(SYSTEM /opt/sdk/include) # a comment (with parentheses)\n"))))
    (is (= ["/home/l/proj/native/include" "/home/l/proj/third" "/usr/include" "/home/l/proj/lib" "/home/l/proj/quoted"]
           (mapv :folder (folders "compile_commands.json"
                                  (str "[{\"directory\": \"/home/l/proj/build\","
                                       " \"command\": \"/usr/bin/c++ -I/home/l/proj/native/include -I ../third"
                                       " -isystem /usr/include -o x.o -c ../native/src/filter.c\","
                                       " \"file\": \"../native/src/filter.c\"},\n"
                                       " {\"directory\": \"/home/l/proj/build\","
                                       " \"arguments\": [\"c++\", \"-I\", \"/home/l/proj/lib\", \"-iquote../quoted\", \"-c\", \"x.cpp\"],"
                                       " \"file\": \"x.cpp\"}]")))))
    (is (= ["include" "third"] (mapv :folder (folders "compile_flags.txt" "-Iinclude\n-I\nthird\n-DDEBUG\n"))))
    (is (= ["include" "../shared"]
           (mapv :folder (folders "Makefile" "CFLAGS += -Iinclude -I$(SDK)/include\nall:\n\tcc -I ../shared -o app main.c\n"))))
    (is (= ["include" "gen"] (mapv :folder (folders "meson.build" "inc = include_directories('include', \"gen\")\n"))))))

(deftest lexes-raw-strings-and-digit-separators
  (let [lines ["auto json = R\"json({\"id\": \"}\"})json\";"
               "auto query = R\"(select *"
               "from t where x = ')')\";"
               "long big = 1'000'000; char brace = '{';"]
        tokenized (lexer/tokenize-lines lexicons/cpp lines)]
    (is (= lines (mapv #(apply str (map second %)) tokenized)))
    (testing "a raw string's braces and quotes stay inside it, across lines too"
      (is (not-any? #(= :bracket (first %)) (nth tokenized 0)))
      (is (= [:string "R\"(" ] (last (butlast (remove #(= :space (first %)) (nth tokenized 1)))))
          "the opening keeps its parenthesis")
      (is (not-any? #(= :bracket (first %)) (nth tokenized 2)))
      (is (= [:text ";"] (last (nth tokenized 2)))))
    (testing "apostrophes group digits"
      (is (some #{[:number "1'000'000"]} (nth tokenized 3)))
      (is (some #{[:string "'{'"]} (nth tokenized 3)))))
  (testing "the sample reads whole"
    (let [lines (source/split-lines (source/normalize-text (fixtures/read-text "test-resources/c-family/sample.cpp")))]
      (is (= lines (mapv #(apply str (map second %)) (lexer/tokenize-lines lexicons/cpp lines)))))))
