# Changelog

## 0.4.1, pre-alpha

### 0.4.1-4

- The inspector minimizes to its header, the page's or folder's name, with a button beside its close button, and a click on the name shows the details again. The choice is remembered.
- While a file is edited, the caret stays. A press anywhere in the cloud, whether it moves the caret, orbits, or lands on the page's name band, leaves the keyboard with the code, and a click on the lifted page off its code leaves the page lifted. With the keyboard elsewhere, in VS Code's editor or on a panel's button, the caret rests faint and steady where typing resumes, and a click on the code takes the keyboard back.
- A click into code stops a camera flight in progress, such as the draw back after zooming out of a file, so the code stays the size it was clicked at. The cloud's first framing no longer overrides a view the viewer has already chosen.

### 0.4.1-3

- The REPL pane, opened with `R` or the toolbar's REPL button, holds a REPL's log and a line to type into, about 350 by 225 pixels, in either sky's colors. Its header drags it and its corner resizes it. Opening it connects to the nREPL server whose `.nrepl-port` or `.shadow-cljs/nrepl.port` file sits in the cloud's folder or a folder above it, and a port typed into it, such as `7888`, connects there. Enter evaluates once the input's brackets and strings close, the arrow keys bring back earlier inputs, and Ctrl+C with nothing selected interrupts.
- While a Clojure file is edited in the cloud, Ctrl+Enter, or Cmd+Enter on macOS, evaluates the selection or the form at the caret, Alt+Enter the top-level form, or inside a `(comment ...)` the form there, and Ctrl+Shift+Enter loads the whole file. Code goes in the namespace its file names, marked with its file and line. Each result shows after the last line of its code, as a value or an error's message, until the file is edited in the cloud.
- The extension speaks nREPL itself, with no other extension needed, and keeps one connection, holding a session of its own, for each open cloud. Closing the cloud closes the session.
- A click on the page being edited keeps the keyboard with the code. It used to move the caret and hand the keys to the cloud, so typing after a second click went nowhere.

### 0.4.1-2

- Each page draws one line to each page it depends on. A C or C++ file that calls a function its header declares and its source defines draws its line to the header, and the use still counts toward the source's definition, its importance, and Most used. Lines into a header or a module interface are dashed, with a legend entry.
- Hidden folds each header, inline file, and module interface that has a partner into that partner: its lines and uses join the partner's, and the partner's measurements count them. A header without a partner, such as a header-only template, holds code found nowhere else and stays.
- Beside sources sets a header next to its source only within one folder, so the public headers under `include/` keep their own folder's cloud while still folding into their sources and counting toward them.
- A row keeps its top left corner while one of its pages is edited, so a growing header pushes its source along instead of covering it.
- Ctrl-click, or Cmd-click on macOS, a name in readable code to fly to its definition: on the page itself, or on the pages it uses, so a C++ call lands on the function's body in its source. When the name could mean several definitions, such as overloads, a menu lists them, and when no page shown defines it, a notice says so. The code tooltip names the key.
- The inspector lists a C or C++ page's neighbors under Uses and Used by, since they include the files reached through headers.

### 0.4.1-1

- A C or C++ header is a page of its own. It still pairs with its source file, the one beside it or the only source of its name in the cloud, and the arrange panel's new Headers choice sets header pages beside their sources, on their own in their folders, or hidden. The choice is remembered for each cloud.
- Beside its source, a header and any inline file of its name sit in a row with the source, declarations first and top edges aligned, on the source's folder cloud. Folders and Relations place each row as one page, and ranked walls rank every page alone.
- A use through a header counts toward the source beside it: calling a function a header declares and its source defines draws a faint line to the source and counts as a use of the source's definition. With headers hidden, these lines join the sources directly. A source's own uses of its header count as none.
- C++ modules are read. Files ending in `.cppm`, `.ixx`, `.mpp`, `.cxxm`, `.ccm`, and `.c++m` are C++. Imports of modules, partitions, and header units become lines, an implementation unit draws a line to its module's interface, and the interface sits beside its implementation as a header sits beside its source. tree-sitter's C++ grammar predates modules, so CodeCloud reads module statements from the tokens and blanks them and `export` before the grammar reads the file, and definitions inside `export` blocks read as usual. `import` and `module` color as keywords.
- The view's state changes route through a table of handlers built from a few shared shapes, in place of two multimethods.

## 0.4.0, pre-alpha

- C and C++ are read in depth. Includes become lines between pages, found beside the including file, in the include folders that `compile_commands.json`, `compile_flags.txt`, `CMakeLists.txt`, Makefiles, and `meson.build` name, in the folders of the cloud file's new `:include-paths`, or at the nearest path ending the same way. System headers count as outside the cloud.
- Definitions come from tree-sitter's C and C++ grammars, shipped with the extension as WebAssembly, and keep the names the code writes, such as `Ring::operator[]` and `TEST(RingTest, Grows)`. Static functions and those in an unnamed namespace are private.
- A header shares a page with its source file: the one beside it, or the only source of its name in the cloud. A file holding `main` keeps its own page, and a `.h` file reads as C++ when its code is C++.
- C and C++ definitions take the same complection measurements as the other languages, with preprocessor conditionals as branches, threads and coroutines as asynchronous work, and `static`, `volatile`, locks, and atomic writes as changes to state.
- C and C++ form one family with a deep magenta in the Language layer, and their files can be edited from the cloud. Raw strings and digit separators lex as C++ writes them.
- The Editor theme colors switch in the arrange panel draws code in the syntax colors, font styles, and bracket pair colors of the editor's theme, with the editor's token and color customizations, in place of CodeCloud's own. Each color moves in lightness only as far as it must to read on the sky. The switch is the new `codecloud.editorColors` setting.
- Reading a codebase takes less time: operators merge and names chain once per file, and a header is lexed a second time only when it may be C++.

## 0.3.3-3, pre-alpha

- Code can be edited on its page in the cloud. Once a page's text is large enough to point into, a click puts the caret there, and typing, selecting by keyboard or pointer, indenting, commenting, undo, and the clipboard work as in an editor. Ctrl+S saves. The pop-up form editor is gone.
- Each change goes straight into the file's document in VS Code, unsaved, so an editor showing the file follows along. A dot after a page's name marks unsaved changes, and the cloud reads unsaved files as they stand, so lines between pages follow the code at each pause in typing.
- While a file is edited, its page grows and shrinks with its lines from its top left corner, and the rest of the cloud holds still until editing stops.
- Ctrl+- zooms out of any file a cloud covers toward the cloud, as Ctrl+Alt+- does in any source file, and after the cloud opens a file the status bar names the key that leads back. Returning to the cloud's tab another way eases the camera back out of the page.
- Folder clouds gather their pages into balls and spread over a rounder bank, with billows all around, so they look round from every side.
- Every folder cloud takes a light tint of its folder's color, a stronger one while Folder is the only color layer, and shading along its billows gives it an edge. Tinted clouds in the arrange panel turns the tint off.
- A twilight sky holds the sunset, with night pages. Pages and panels turn early in any dusk, so an evening under the clock sky never leaves them half gray, and the moon rises once the dusk has passed.
- Tabs in code reach four-column stops in text and minimaps alike.

## 0.3.3-2, pre-alpha

- The legend is a key to what is in view: the layers turned on, then cyclic dependencies, cyclic imports, parse errors, and the kinds of line, each under a short name. The explanatory sentences are gone from the legend, the arrange panel, and the views panel.
- Parse errors are bright red, with a frame that stays at least three pixels wide from afar. Cycles take a deeper red, so the two stay apart.
- Black marks what CodeCloud reads only as text. In the Language layer, C, C++, and the other languages without an adapter share one Unclassified entry, and Importance and Complection color those pages black, since only an adapter could measure them.
- The Length layer runs from pale to deep violet, so red always means a problem.
- Lines between JavaScript and TypeScript count as lines within one language family, as the graph marks them.
- Tooltips count a page's uses among the pages shown, as the inspector does.
- Text and name bands redraw when the editor font or a page's shape changes.
- The host drops an analysis or a configuration read that a newer one has replaced, so quick edits to the cloud file cannot leave the cloud on old settings.
- Commands wait for the first search for cloud files, so a command that activates the extension finds the clouds already in the workspace.
- Path helpers, a page's folder, reference counts, and the Clojure platforms each live in one place.

## 0.3.3-1, pre-alpha

- The views panel turns pages on and off: a box for each language, and a tree of folders and pages where a box marked with a dash holds some hidden pages.
- Hidden pages leave the cloud. The pages still shown are measured among themselves, so uses, dependents, importance, folder colors, and the layout all follow what shows. The inspector counts hidden neighbors, and a dashed chip brings a hidden page back.
- Clicking a dot in the legend or the inspector opens a color chooser for a folder, a language, or the end of a ramp, with swatches that read on both skies, a custom color, and the default. Chosen colors are remembered for each cloud file.
- The interface is set in Recursive, slightly lighter and leaning a little toward monospace. The title keeps Nunito.
- Arriving from the editor at a hidden page shows that page again and leaves the rest of the filter in place.

## 0.3.2-2, pre-alpha

- Night folder clouds stay moonlit blue and lean toward their folder's hue. Warm folders no longer turn pink beside the cycle reds.
- Each page owns its minimap, so none is freed while in use or kept after its page closes, and the highlight cache keeps its most recently used entries.
- Hovering redraws only what changes. Page shapes and labels wait for the layout to change, the tooltip is measured only when its subject changes, and pages at rest no longer recompute their places every frame.
- Pages whose text never arrived ask again after the snapshot changes.
- A file a language adapter fails to read becomes a plain page with a parse error, and the rest of the cloud still analyzes.
- Closing the last panel of a cloud frees its analysis caches.
- Holding the zoom-out key counts every step, so the editor's font returns to its size.
- Shorter docstrings and interface text, and the legend lists its keys as a table.

## 0.3.2, pre-alpha

- Color layers take the place of color schemes. Language, Folder, Dependents, Importance, Complection, and Length each have a checkbox in the arrangement panel, and a page blends the colors of every layer turned on, mixed in Oklab. With every layer off, pages are gray.
- The Folder layer colors a page by the folder its file is in. Folders take hues spaced around the wheel in file tree order, so subfolders follow their parent. The Dependents layer gives a page the folder colors of the pages that use it.
- The cloud file's `:color-layers` adds layers that shade pages by a measurement or color the pages matching patterns.
- The inspector lists what each layer says about a page, and the legend explains each layer turned on.
- The title's totals are a table of pages and lines for each language, opened from **Totals**.
- Pages in a dependency cycle carry ↻ beside their names, and no mix of other colors comes out the red of a cycle.

## 0.3.1, pre-alpha

- Every file of a known language becomes a page. Python, JavaScript, TypeScript, CSS and its Sass and Less dialects, HTML, and shell scripts read in depth, and C, C++, Java, Kotlin, C#, Go, Rust, Ruby, PHP, Lua, R, and Julia show as highlighted pages. A script without an extension counts when its shebang names a known program, and binary files, minified bundles, and files of more than a million characters stay out.
- Each page highlights in its own language, with HTML reading the scripts and style sheets inside it, and shell reading here documents and command substitutions inside strings.
- Imports become lines between pages: Python imports, JavaScript and TypeScript imports, requires, and re-exports, style sheet imports, the scripts, style sheets, and pages HTML loads and links, the scripts shell sources and runs, ClojureScript requires of local JavaScript, Clojure imports of project Java classes, and the scripts a Babashka `shell` call runs. Resolution follows each language's rules, including `tsconfig.json` path aliases, packages of the same repository, Sass partials, and Python source folders, and the inspector lists imports that reach nothing in the cloud.
- A **Language** color scheme gives each language its own hue, the same in every cloud, and draws lines from one language to another heavier. In a cloud of several languages each name band carries a language tag, and the title counts modules in so many languages.
- Import loops that Python, JavaScript, and shell allow show with amber edges and an explanation in the inspector, while Clojure's dependency cycles stay red.
- Python, JavaScript, TypeScript, Sass, and shell definitions are listed, counted, and measured for complection with the same thresholds as Clojure, uses of imported names count toward the definitions they reach, and their top-level statements, rules, and functions can be edited from the cloud.
- The cloud file's new `:languages` key narrows a cloud to some languages, and `:extensions` lists exactly the file types to read. The inspector, tooltips, search, and title use each language's own words.
- The zoom keys and the title bar's cloud button work in every known language's files, the cloud's code takes the font settings of the language being edited, and a new cloud is offered in the nearest folder with a project file of any known language.
- Clojure is read as before, through the same adapter interface as every other language, and a cloud that holds only Clojure analyzes exactly as it did.

## 0.3.0, pre-alpha

- Search lists the matching namespaces under the field, most important first. The arrow keys choose among them, and Enter or a click brings one closer. The field takes focus as soon as `/` opens it, so the first keys typed land in it.
- A `:complection` key in the cloud file sets how much of each measurement a definition may have before it counts, such as `{:lines 40 :branches 8}`.
- Macros are measured by their syntax-quoted templates as written. Before, the expansion of a template counted, which made short macros look deeply nested.
- The title names the arrangement and color scheme whenever they differ from folders colored by folder, and clicking that line opens the arrangement panel.
- **Usual spacing** in the arrangement panel returns the spacing to normal, and `?` opens or closes the legend.
- The legend stays clear of the title.
- The overlay is split into one namespace per panel, page drawing into minimap tables, page text, and pages, and the decision of how solid each page is drawn moved into the presentation layer, where tests cover it.

## 0.2.0

- Arrangements, chosen from the new arrangement panel (`A`). **Folders** sets every folder in a cloud of its own, apart from the others, with pages leaning toward the namespaces they use, and **Relations** places pages by their dependencies alone. Five ranked arrangements line the pages up on a wall by length, functions, uses, dependencies, or complection, each page numbered by its place.
- Complection, which measures how tangled each definition is from its length, branches, nesting, asynchronous operations, changes to state, and anything the cloud file's new `:flags` key names. The inspector lists a namespace's most complected definitions with their reasons, and the command-line summary lists the most complected namespaces.
- Page color by folder family, with tests sharing the hue of the code they test and every page leaning toward the hues of its neighbors, as the new default. Importance and complection are the other color schemes.
- Dependency lines that fade from one page's color to the other's.
- Folder clouds drawn as distinct cumulus, lit from above and tinted toward their folder's hue, shown in the Folders arrangement.
- A spacing control that opens the cloud out or closes it up around the point the camera circles, also on `[` and `]`.
- Pages that can be carried somewhere else with Ctrl-drag or Alt-drag and put back from the right-click menu.
- A sky with clouds across the whole background, a low sun by day, a moon by night, and the occasional flock of birds or airplane.
- The arrangement, color scheme, and spacing are remembered for each cloud file.
- Flagged symbols match however a file's aliases and refers spell them, and nesting counts code forms only, so data literals and binding vectors no longer read as deep code.
- Keys typed right after opening search go into the search box instead of triggering shortcuts, and the framing of the whole cloud leaves room for an open side panel.

## 0.1.0

The first release.

- Namespaces drawn as pages of their own code at the editor's scale, gathered into folder clouds and joined by dependency lines, with importance, cycles, and unparseable files marked by color.
- Cloud files (`*.cld`) that open as clouds and hold `:include`, `:ignore`, `:views`, `:weights`, and `:extensions`, with `:codecloud/config` in `deps.edn` as an alternative.
- Zooming out of the editor into the cloud past a configurable threshold, and scrolling into a page to open its file at the line under the pointer.
- Bringing a page or a whole folder closer with a click, views and personal filters, a names-only mode, search, and a history timeline.
- Editing a top-level form from the cloud, saved through VS Code.
- Day, night, and time-of-day skies.
