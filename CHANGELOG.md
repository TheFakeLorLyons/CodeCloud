# Changelog

## 0.4.3, pre-alpha

### 0.4.3-6

- The cloud answers Ctrl+Alt+R, Cmd+Alt+R on macOS, itself, before VS Code and any extension's keybindings see the keys, so **Run a command…** opens for the page in hand wherever the keyboard is in the cloud, a box in a pane included. `C` opens the same list while nothing is typed.
- **Ignored**, at the foot of the Views pane and closed until opened, lists what the cloud leaves out, from what git ignores and the folders CodeCloud always passes over to what the cloud file's `:ignore` names and files of a kind CodeCloud does not read, each saying which. A folder opens to what it holds, and a box brings a folder or file into the cloud, where its pages show gray in a plain cloud and the inspector says why. Each cloud remembers the choice, and its own files come first under the file limit.

### 0.4.3-5

- **CodeCloud: Run a Command for the Page**, Ctrl+Alt+R or Cmd+Alt+R in the cloud, offers every command VS Code's extensions contribute and runs the one picked for the page in hand, the page being edited, brought closer, or under the pointer, as **Run a command…** in a page's right-click menu does, which now shows those keys beside it. From VS Code's own editor, the command offers the same list for the file there.
- The Views pane lists the open windows after its folders. Unchecking a window hides it from the cloud while it stays open, remembered with the cloud's windows, and a hidden window pinned to the grid still shows in its cell. A click on a window's name, there or in the windows menu, shows it again and flies to it.
- `T` switches **Auto-hide the toolbars**, and the switch says so, as the Arrange pane's spacing now names `[` and `]`.
- Three numbers used in one place each moved into the function using it, the Oklab conversion works out one weight of each row from the other two, since its rows sum to one, and the helper for an outcome without effects says that it takes a changed state.

### 0.4.3-4

- **Auto-hide the toolbars**, a switch in the Views pane, hides the toolbar, the cloud's name, and the timeline until the pointer reaches the top or bottom edge of the view, as a taskbar hides, and they slide back a moment after it leaves. The grid fills the whole view meanwhile, and the bars return on glass over it. A bar holding the keyboard's focus, such as the toolbar while its address field is typed in, stays, and the choice holds for every cloud.
- wavescope's reads around a line, `query_wavelet_context` and `get_cursor_context`, give agents the exact lines nearest the line alone, leaving out the medium and coarse bands and the peaks, which mostly repeat those lines, and so cutting such a read by about half. **Add the medium and coarse bands**, in the Coworkers pane, or `codecloud.agents.wavescopeBands` set to `all`, brings them back for a large codebase, and connected agents list their tools again with descriptions to match.
- `get_summary_at_scale` lists its lines in order of line. wavescope listed a summary's peaks by strength, with the gaps between them marked in that order, and the hub sorts the peaks and marks the gaps again.
- The title tells the grid where it ends even while it hides, so the grid's top returns below the title when the switch is turned off.

### 0.4.3-3

- A page's right-click menu runs commands for its file. **Run a command…** lists the commands VS Code's extensions offer, labeled as the command palette labels them, and runs the one chosen with the page's file in the editor group holding the cloud, its caret where the cloud's caret stands or at its first line, then brings the cloud back. Commands that act on the editor's file, as Calva's and Calva Power Tools' **Clay: Watch file** do, so act on the page.
- The `codecloud.pageCommands` setting adds commands of your own to every page's menu, each a VS Code command with any arguments a keybinding would give it, or a command line run in a terminal of its own with `${file}`, `${relativeFile}`, and `${line}` put in. A command naming a `window` shows that address, or that port on this computer, in a window once it runs, and running it again reloads the window.
- A window for a server on this computer waits while nothing answers at its port, trying each second for five minutes and saying what it waits for, so a window opened beside the command that starts a Clay notebook, a Jupyter server, or any other server shows the page once the server is up.
- A port alone, such as `1971`, stands for `http://localhost:1971` in the windows menu's field and in **Show a Web Page in the Cloud**, with any path after it, as in `8000/docs`.
- A web address in the REPL pane's output is a link, and a click shows its page in a window.
- Agents' badges, pins, bands, glows, and change chips show on pages pinned to the grid, kept inside their cells.
- The Views pane's **Color the toolbar's icons** switches the icons at the top right between their own colors and the text color, white on a dark sky, and the choice stays in `codecloud.toolbarColors`.
- A window under the pointer no longer opens an empty tooltip beside it, and **Open in the Cloud** finishes as soon as it shows its notice, so a page command running it brings the cloud back at once.

### 0.4.3-2

- Grid cells rearrange and resize. Dragging a cell's header onto another cell trades their places, and the cell under the pointer shows an outline before the drop. The gaps between cells are borders, dragged to share the width between the columns or the height between the rows, and a double-click on one evens the cells beside it. The grid keeps its borders with its cells for each cloud.
- Simple takes a color of its own. The row at the foot of the sky button's list picks it, and the `codecloud.simpleColor` setting keeps it. The chosen color fills the sky overhead and brightens toward the horizon, pages take their day look on a light color and their night look on a dark one, and **Default** returns Simple to the editor theme's colors.
- The **Night Library** joins the environments, a maze of dark wood paneling, red carpet, and warm lamps with night pages.
- A maze's walls are boxes 16 centimeters thick with posts where they meet, so a wall seen end on shows its end where a line one pixel wide stood before.
- The icons at the top right each take a color of their own. The `codecloud.toolbarColors` setting draws them plain, in the text color, or names colors by icon, `{"all": "#ffffff"}` making them all white.
- The Views pane holds two switches at its top, **Show names only (N)**, which moved there from the toolbar, and **Hide totals and hidden folders**. `N` still switches names on and off.
- The Arrange pane's last button reads **Reset to default** and puts back everything the pane changes, the arrangement, the headers, the colors, the spacing, and pages moved by hand. The sky's right-click menu offers **Reset page positions** and a page's menu **Reset its position**.
- The assistant's pane says that it offers insight into the code while agents connected through MCP can also edit it.
- The dock's borders, the grid's borders and headers, and a window's bar keep the pointer while dragged, so crossing a window's page no longer drops the drag.

### 0.4.3-1

- Windows float in the cloud beside the code, each showing a web page, an HTML file with the scripts, styles, and pictures beside it, Markdown, a PDF, or an image. The toolbar's windows button opens a menu with a field for a web address, a choice of file from the workspace folder, and the open windows, and VS Code opens them through **Open in the Cloud**, in the right-click menus of the explorer and of an editor's title, and **Show a Web Page in the Cloud**. Each cloud remembers its windows and where they float.
- A window faces the camera, and shows one pixel to a pixel when the code around it shows at the editor's size. A click lets the pointer through to it until the pointer leaves, its bar drags it, and its buttons reload it, pin it to the grid, open it in VS Code or the browser, and close it. A double-click flies to it, showing the whole cloud takes in the windows, and a window too far off to read wears its title as a label, as pages do.
- Windows stay current. Markdown follows its editor as it is typed in, an HTML page reloads when any file in its folder changes, and PDFs and images reload with their files. A web page's address passes through VS Code, so a local server in a remote workspace arrives through a forwarded port. An HTML file comes from a server that listens only on 127.0.0.1 and answers only under a random path, so other pages cannot reach the folder. pdf.js, which ships inside the extension, draws each page of a PDF as it scrolls near the view.
- The grid pins up to four pages or windows to the screen, laid out as VS Code lays out editor groups. One fills the view, two share it in columns, a third stacks with the second on the right, and a fourth makes two rows of two. **Pin to the grid**, in the right-click menu of a page or a window, adds one, `G` or the toolbar's grid button shows and hides the grid, and each cell's header sends its page or window back to the cloud. Each cloud remembers what its grid holds.
- A pinned page shows its code at the editor's size in one column, scrolls under the wheel, and edits as anywhere in the cloud, and a flight to one of its lines, from Go to definition or a place the assistant names, scrolls its cell there. A pinned window works as it does in the cloud, and resting the pointer on its cell lets the pointer through.
- Unchecking **Totals and hidden pages under the name**, a box in the Views pane, hides the totals and the list of hidden pages under the cloud's name, and the choice holds for every cloud.

## 0.4.2, pre-alpha

### 0.4.2-10

- A color layer in the cloud file can shade pages by how often one of its flags matches, as `:database-calls {:flag :database :colors ["gray" "purple"]}` does, so a question such as which namespaces make the most database calls takes a flag and a layer. Matches shade along the logarithm, as lengths do, so a page with a few shows beside one with many.
- `:weights` in the cloud file weigh any measurement toward importance, such as `{:dependents 0.5}`, where only functions, uses, and lines counted before. The measurements live in one map, `codecloud.model.metrics/measurements`, and one added there is one `:weights` and `:color-layers` accept.
- The timeline gives a commit's age as the Coworkers pane gives an agent's, in minutes, hours, days, months, or years, from the one `codecloud.counting/ago`.
- The README's **Making your own** section names where each addition goes, from a room in settings and a flag in the cloud file to a backdrop, a measurement, an arrangement, a tool for agents, and a language.
- Helpers copied between namespaces gave way to shared ones, `codecloud.io.task/coded-error` for the errors of the MCP and nREPL clients, `codecloud.render.planes/pooled-mesh!` and `dynamic-attribute` and `codecloud.render.dom/pooled-element!` and `fit-height!` for what several layers drew alike, and `codecloud.languages.structure/depths` for the nesting of brackets and of shell's words.
- Long functions in the page details tool and the cloud's key handling split into named steps, locals that hid a `clojure.core` function or another namespace's name took names of their own, docstrings that only restated a name are gone, and the longest are shorter.

### 0.4.2-9

- The sky button chooses the environment the cloud floats in, and resting the pointer on it lists every environment with the chosen one checked. A click still steps to the next one, and the choice stays in the `codecloud.sky` setting as before.
- **Simple** is the first look. It draws a plain gradient behind the cloud, each folder as one flat disc, and at most 30 frames a second, so a large codebase or a modest computer starts light, and the skies are a click away. Anyone who never set `codecloud.sky` opens in Simple from now on.
- **Backrooms** draws an endless office around the cloud, room after room of mono-yellow wallpaper and damp carpet under ceiling tiles whose fluorescent panels flicker now and then.
- The `codecloud.spaces` setting describes spaces of a person's own, each one room or rooms in every direction, with a color and a pattern for the walls, the floor, and the ceiling, lamps, a haze for far rooms, folders as clouds, discs, or nothing, and pages in their day or night look. Described spaces join the list under the sky button as soon as the setting changes.
- Environments are data in `codecloud.presentation.environments`, and each kind of backdrop, the sky, the plain gradient, and rooms, is a module of its own in `codecloud.render`, so another environment takes an entry in the list, and a new kind of backdrop one module more.

### 0.4.2-8

- The side panes share a dock on the right of the view, so Views, Arrange, Coworkers, the assistant, and the inspector open together, one above another, where opening one used to close the rest. Each pane's chevron minimizes it to its header and expands it again, dragging the border between two panes shares their height, and dragging the dock's left edge sets its width. The dock keeps its width, the panes' shares, and which panes are minimized between sessions, and Esc still closes the panes one at a time.
- Following an agent frames its line at a lower zoom, so more of the code around it shows. A flight to a line, whether following, Go to definition, or a place the assistant names, puts the line in the part of the view the panes leave, its start clear of the dock and its code centered there when it fits, and above the REPL pane when that pane covers the lower half of the view. A page flown to after a double-click is centered in that part too. When an agent reads a whole file, Follow shows its page whole, and the Coworkers pane names the file without a line.
- The cloud's tools for agents are records, one per tool in a namespace of its own under `codecloud.agents.cloud-tools`, each holding the tool's description for MCP, its answer, the files it reads, and the lines it marks as read. `codecloud.agents.cloud-tools` gathers them in a registry, so a new tool takes one namespace and one entry there.
- Each language's reading of its structure lives in that language's namespace, `codecloud.languages.python`, `javascript`, `css`, and `shell`, and `codecloud.languages.structure` keeps the scanning they share.
- The VS Code host's handlers for the view's messages sit with their feature in `codecloud.vscode.session.snapshots`, `editing-messages`, `repl-messages`, and `agent-messages`, each adding its methods to one multimethod, and `codecloud.vscode.session` keeps the view's lifetime and preferences.

### 0.4.2-7

- Every report an agent makes costs it tokens, so how much agents report is now a choice, made with the **Reporting** buttons in the Coworkers pane or the `codecloud.agents.reporting` setting. **Full** keeps every report, **Agents** leaves subagents quiet, **Brief** has agents announce each task once, without lines, and withdraws pins and the list of coworkers, and **Off** withdraws the reporting tools. Connected agents list their tools again when the level changes, the hub's instructions ask only for what the level wants, and at every level the cloud still shows what agents read through the hub and the lines they change.
- The assistant asks Anthropic to cache the conversation, so each turn pays a tenth or less for what came before, and CodeCloud's output channel logs how many tokens each reply read from the cache, wrote to it, took uncached, and gave back. `codecloud.assistant.cacheLifetime` keeps the cache an hour in place of five minutes.
- The two load commands are **Load/Evaluate Current File** and **Load/Evaluate Current File and Its Requires/Dependencies** wherever they appear, in the command palette, the REPL pane's load button, the keys legend, the chord's status line, and the pane's log.
- A REPL connection closed while it was opening no longer leaves its tab unable to connect again.
- Reading the code again after a save or an edit in the cloud skips what stayed the same. A file whose size and times are unchanged is not read again, a reading that finds every file as it was keeps its snapshot and sends nothing to the view, and resolving C and C++ includes takes half the time. After typing in the cloud, a codebase whose readings take long waits longer before the next. Facts read from history no longer crowd out the working tree's, so browsing many commits never makes the working tree parse again.
- A long reply from an nREPL server, arriving in many pieces, is read once, where each piece made the client read the reply again from its start.
- A shell tab shows the words of programs that place them by column, as Claude Code does, with the spaces between them, and the escapes that save and restore the cursor no longer leave a 7 in the tab and hold back the rest of the output. A program that draws over the terminal in place gets a note in the tab, which brings up its terminal in VS Code's panel, where the program works as drawn. The REPL pane moves back into view when the view grows shorter, as when that panel opens below the cloud, and returns to its place once there is room.
- Namespaces that shared a name across layers carry their layer in it, `-state` for the view's state, `-host` for VS Code, and `-view` for drawing, so the REPL's three are `codecloud.presentation.repl-state`, `codecloud.vscode.repl-host`, and `codecloud.render.hud.repl-view`, and those of the assistant, editing, forms, presence, and the sky follow suit. The model's and the agents' namespaces keep their names.
- Copied helpers gave way to shared ones, `codecloud.io.task/error-text` for error messages, `codecloud.counting` for counts and times in words, and `codecloud.render.planes` for the unit square and the rectangles drawn over pages. The longest docstrings are shorter.

### 0.4.2-6

- With Calva installed, the REPL pane's + menu offers Calva's REPL, a tab that joins the REPL Calva has connected through Calva's extension API, so the cloud and Calva's output window share one session. Code from the tab, and from the pages while it evaluates, runs in Calva's ClojureScript session for a `.cljs` file and its Clojure session for a `.clj` file, and shows in Calva's output window under CodeCloud's name, while what Calva evaluates from its own editors and output window shows in the tab. Code for a namespace the REPL lacks gets its file's `ns` form first, as in other REPL tabs. A tab opened before Calva connects joins once it does, leaves when Calva's last REPL disconnects, and joins again when code is typed into it. Its stop button stops the tab's evaluation on Calva's nREPL server, then runs Calva's interrupt command, which stops the rest of what Calva runs; Calva's command alone would wait for the busy session.
- Evaluation keys follow Calva's, in the cloud and in VS Code's editor. Ctrl+Enter evaluates the form at the caret, Ctrl+Shift+Enter the form enclosing it, and Alt+Enter the top-level form, while Ctrl+Alt+Enter, Shift+Alt+Enter, and Ctrl+Shift+Alt+Enter evaluate the enclosing form, the top-level form, and the file up to the caret, closing the brackets open there. Ctrl+Alt+C Enter loads the file, Ctrl+Alt+C Shift+Enter loads it after what it requires, and Ctrl+Alt+C Ctrl+Alt+D interrupts the evaluations, the top of the cloud saying what the chord's second key does. Ctrl+Shift+Enter and Alt+Shift+Enter no longer load files, and in the cloud Cmd works for Ctrl on macOS.
- VS Code's editor gains Evaluate the Enclosing Form, Evaluate the Enclosing Form up to the Caret, Evaluate the Top-Level Form up to the Caret, Evaluate the File up to the Caret, and Interrupt the Evaluations, on the same keys, which still stand aside while Calva's keybindings are on.
- Esc clears results in VS Code's editor only in a Clojure editor that shows some and has nothing selected, so elsewhere it still clears selections and reaches Vim and Calva's REPL window.
- A line typed into a shell tab while a command runs goes to that command as input, so the tab answers a prompt and talks to a REPL or another program started in the terminal. Before, VS Code's shell integration pressed Ctrl+C to run the line as a command of its own. The tab runs while any command typed in it or in VS Code's terminal panel does, its prompt giving way to the program's. The + menu leaves out terminals an extension draws itself, such as Calva's output terminal, since no shell runs there to take commands.
- Keys the REPL pane, the search box, and the assistant's box do not answer reach VS Code, so the command palette and quick open work while typing there.
- The REPL pane opens wider, so three tabs fit, keeps its + button beside the tabs, shows the active tab when the tabs outgrow the strip, and scrolls them sideways with the wheel.
- Code that moves namespaces itself, such as a selection ending in an `in-ns` form, no longer makes CodeCloud take the REPL for one that ignores the namespace asked for. On nbb, an `in-ns` form typed into the pane moves the tab into its namespace, sent as the `ns` form nbb follows.
- When the connection closes during a load, the file cut off no longer counts as loaded and the files after it wait. The `ns` form sent ahead of code in a namespace the REPL lacks carries its own line in its file, and a file whose `ns` form names another namespace sends nothing in its place. A value the server cut short ends in an ellipsis, and a REPL that cannot interrupt says so beside the evaluation.
- When the pane's own code moves the tab into another namespace, the tab takes the file of the cloud holding it, so a namespace the REPL lacks loads from the right file.
- The REPL's host side has tests of its own against stand-in servers answering as Clojure's nREPL and nbb do.
- Docstrings describe in sentences what they once listed after a colon, the longest are shorter, and the view's handlers share their helpers from `codecloud.presentation.handling`. Two functions nothing used are gone.

### 0.4.2-5

- Brackets and double quotes typed in the cloud come in pairs, the caret between them, and typing the closing one where it stands steps over it. Typed over a selection, they wrap it, and Backspace between an empty pair takes both. Strings, comments, and the character after a backslash take every character as typed.
- Enter starts a Clojure line where cljfmt puts it, as Calva's formatter does, with the bodies of `defn`, `let`, and their kin two spaces in, the arguments of a call under its first, and the elements of a collection inside its bracket. Inside a string, the new line lines up under the opening quote, as docstrings do.
- Tab formats the top-level form at the caret, and Shift+Alt+F the whole file, through VS Code's formatter, which for Clojure is Calva's when it is installed, following the project's cljfmt settings. Without Calva, CodeCloud indents Clojure as cljfmt does by default. A format is one change that undo takes back, and Tab still indents a selection and code in other languages.
- The bracket beside the caret and its partner are tinted, and a pair of different kinds, or a bracket without a partner, turns the color of errors.
- The problems VS Code knows of in the cloud's files underline the code they cover, red for errors, amber for warnings, and blue for information, which brings in clj-kondo's findings through Calva or clojure-lsp. Resting the pointer on such a line lists its problems, even on the page being edited.
- A new file button on the toolbar, and **New file** in the menus of pages, folders, and the sky, make a file, named in VS Code's input box starting in the folder at hand, with any missing folders made. A Clojure source file starts with an `ns` form naming the namespace its place implies, read from where the cloud's namespaces keep their files, and the cloud flies to it with the caret below that form. A file the cloud shows read-only opens in VS Code's editor instead.
- An empty file can be edited in the cloud, its page holding one line for the caret.
- Code from a Clojure file without an `ns` form runs in `user`, or in `cljs.user` for a `.cljs` file, as Clojure runs a script, and loading such a file moves into that namespace first. Before, it ran in whichever namespace the REPL held last.
- A form left unclosed no longer stops evaluation elsewhere in its file. A form opening at the first column of a line ends any form left open above it, so the forms below it still evaluate. An error printed as `null`, as a StackOverflowError is, shows its class.
- Esc takes away evaluation results and notices before it stops editing, and in VS Code's editor it clears the results shown after the code.

### 0.4.2-4

- The cloud gains an assistant. `Q` or the toolbar's assistant button opens its pane at the right of the sky, where a question about the codebase goes to the model the person picked, which reads the code through the agent hub's tools before it answers. The reply streams in as Markdown, with lists, tables, and code, and a place written as `path:line` flies there when clicked, the line centered beside the pane.
- **CodeCloud: Choose the Assistant's Model**, also behind the pane's model button, lists the models each provider offers for its key, marks the one in use, and adds or removes providers. Anthropic, OpenAI, and OpenRouter take an API key; Ollama and LM Studio on this computer take none; and any other server speaking OpenAI's chat completions API joins at the address typed for it, with a key when it asks for one.
- A key goes into VS Code's own password box, never the cloud's view, and VS Code's secret storage keeps it out of the settings and the repository. Removing a provider removes its key. The providers and the model chosen hold for every workspace.
- The assistant reaches the hub as one more MCP client, named for its model, so it wears a badge, lays bands over what it reads, and appears in the Coworkers pane and the hub's log like any agent. Each tool it calls shows in the pane as a row saying what the call was about, with what the tool answered behind a disclosure. It reads and explains, changing no files, and after 24 rounds of tools in one question it stops to let the person ask it to go on. The stop button ends a question, keeping what was said, and the new conversation button starts over. With the hub off, it answers without tools, and the pane says so.
- A flight to a line, as from the Coworkers pane, centers the line in the part of the view that no side pane covers, and framing the cloud leaves room for the Coworkers pane as it does for the others.

### 0.4.2-3

- The cloud shows where agents work. Each agent, named from its client, wears a badge of its initials in a color of its own. The badge flies to the line the agent read, announced, or changed last, with a line saying what it is doing. A read through the hub lays a band over the lines it took in, fading over four minutes.
- A change no person made glows on the lines it changed for a quarter hour, with a mark in the gutter, and a chip at the page's corner counts the lines added and removed. Changes come from edits to open documents that neither typing at the caret, an undo, a save's formatting, nor the cloud made, and from writes to disk that no save in VS Code explains, as when an agent in a terminal edits a file. CodeCloud learns the text of each file an agent reads or announces, so the changed lines are found even for files no editor holds.
- Each change goes to the agent that read or announced its file last, within ten minutes, or else to an unnamed agent, and the chip, the pane, and the `coworkers` tool say the credit is a guess. The hub's log in the CodeCloud output channel names each change and its agent.
- Three tools join the cloud's own, nine in all. `announce_work` shows the person which file the agent works on and what it intends, and a subagent passing its own name appears on its own. `point_out` pins a note to a line, which the person clicks to fly there; agents never move the camera. `coworkers` lists every agent at work with its file, its last read and change, and its intent. The hub's instructions ask agents to announce their work and to prefer `page_details` and `uses_of` to reading whole files.
- The agents button opens the Coworkers pane, which lists the agents with what each is doing and where, their pins, and what they did lately, each a way to fly there, and connects another agent. **Follow** keeps the camera on one agent until the person moves the camera, and Escape stops it. A notice appears when an agent changes a file open in the cloud.
- A shell tab closed within a few seconds of opening, before shell integration started, no longer raises an error in the extension host.

### 0.4.2-2

- The agent hub serves six tools of the cloud's own beside wavescope's. `cloud_overview` gives the codebase's folders, its most important pages, its cycles, and the files that do not parse; `page_details` one page's definitions, dependencies, and tangles; `uses_of` every line using a definition, with its code, read through aliases and refers in Clojure and by name elsewhere; and `path_between` the chain of requires tying two pages. `cloud_view` and `viewer_focus` tell agents what the person's filter shows, where their caret and selection are, which page they lifted, and which pages are on their screen.
- The cloud's view reports what the person is looking at about once a second while it changes, and the hub follows the caret in VS Code's editors as well. wavescope's `get_cursor_context` and `get_cursor_important_positions` read around that caret, in the caret's file unless the agent names another the caret has been in. With no cloud open, the cloud's tools answer from the workspace, read in the background.
- The REPL pane holds tabs, each a REPL with a connection of its own or a shell in a VS Code terminal, added from its + button. A shell tab opens a terminal in the cloud's folder or joins one already open, reads each command's output, exit code, and working folder through VS Code's shell integration, and shows commands typed into VS Code's terminal panel too.
- The pane follows the page last clicked, its prompt naming that page's namespace, and code typed there runs in it. When the REPL lacks that namespace, the file's `ns` form goes first and brings in what it requires, and an unresolved symbol the file defines comes with a note to load the file. Alt+Shift+Enter, or the load button in the pane's header, loads a file after the files of the namespaces it requires, each once, in loading order.
- The pane resizes from any edge or corner and keeps its place and size, and it opens with the first evaluation from a page. Evaluations echo their whole form in the pane, up to twelve lines.
- VS Code's own editor evaluates Clojure files inside a cloud through that cloud's REPL, with commands for the form at the caret, the top-level form, the file, and the file after what it requires. Each result shows after its code until the file changes, the pane echoes the code and follows the file's page, and the keys, Ctrl+Enter, Alt+Enter, Ctrl+Shift+Enter, and Alt+Shift+Enter, stand aside while Calva's keybindings are on.
- nbb runs code wherever its last `ns` form left it, whatever namespace a request names, so CodeCloud moves nbb with an `ns` form before evaluating in another namespace. Another server found doing the same gets a note in the pane and the same treatment from then on.

### 0.4.2-1

- An MCP server, the agent hub, runs inside VS Code and gives AI agents five tools from yogthos's wavescope-mcp, which reads code by its structure at three resolutions. The tools show a file around a line, summarize a range, find the landmarks of a file or the whole workspace, compare a file's structure between git revisions, and score each line's density. wavescope ships inside the extension as one bundled file and runs under VS Code's own Node.
- The hub speaks MCP's Streamable HTTP transport on 127.0.0.1 and accepts only requests carrying its token, a random 256-bit value that VS Code's secret storage keeps. It refuses requests from web pages and paths outside the workspace's folders, and each workspace keeps its port, so connected agents find the hub after VS Code restarts.
- **CodeCloud: Connect an Agent** connects Claude Code by running or copying `claude mcp add`, opencode and Codex by copying an entry for their configuration files, and any other MCP client by copying the URL, the token, or a JSON configuration. VS Code's own agents find the hub by themselves from VS Code 1.101 on.
- The status bar counts the agents present and names them in its tooltip, and the cloud's toolbar gains an agents button that does the same and opens Connect an Agent.
- Three settings place the hub: `codecloud.agents.enabled`, `codecloud.agents.address`, which reaches agents in virtual machines or on other computers, and `codecloud.agents.port`.

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
