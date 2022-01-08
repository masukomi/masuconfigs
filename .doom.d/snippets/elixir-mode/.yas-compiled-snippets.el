;;; Compiled snippets and support files for `elixir-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'elixir-mode
                     '(("dhp" "def handle_params(${1:params}, ${2:_url}, socket) do\n  $0\n  {:noreply, socket}\nend\n" "LiveView def handle_params" nil nil nil "/Users/kayrhodes/.doom.d/snippets/elixir-mode/dhp" nil nil)
                       ("dhi" "def handle_info(:${1:message}, socket) do\n  $0\n  {:noreply, socket}\nend\n" "LiveView def handle_info" nil nil nil "/Users/kayrhodes/.doom.d/snippets/elixir-mode/dhi" nil nil)
                       ("dhe" "def handle_event(\"${1:event}\", ${2:_params}, socket) do\n  socket =\n    assign(socket,\n      ${3:key}: ${4:value}\n    )\n  $0\n  {:noreply, socket}\nend" "LiveView def handle_event" nil nil nil "/Users/kayrhodes/.doom.d/snippets/elixir-mode/dhe" nil nil)
                       ("defrender" "def render(assigns) do\n  ~L\"\"\"\n  $0\n  \"\"\"\nend\n" "LiveView def render" nil nil nil "/Users/kayrhodes/.doom.d/snippets/elixir-mode/defrender" nil nil)
                       ("defmount" "def mount(_params, _session, socket) do\n  socket =\n    assign(socket,\n      ${1:key}: ${2:value}\n    )\n  {:ok, socket}\nend\n" "LiveView: def mount" nil nil nil "/Users/kayrhodes/.doom.d/snippets/elixir-mode/defmount" nil nil)
                       ("deflivemod" "defmodule $1.$2Live do\n  use $1, :live_view\n  $0\nend" "LiveView: def module" nil nil nil "/Users/kayrhodes/.doom.d/snippets/elixir-mode/deflivemod" nil nil)))


;;; Do not edit! File generated at Fri Dec 10 10:30:12 2021
