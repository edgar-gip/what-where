# what-where

_Find what you look at and where you are._ \
Edgar Gonzàlez i Pellicer, 2017&ndash;2025

## Description

`what-where` is an Emacs Lisp extension which provides contextual actions
according to the element under the cursor. It offers an extensible and unified
framework, in which different _providers_ suggest _items_ that allow _actions_,
with items automatically ranked based on previous user selections.

## Installation

In order to use `what-where`, clone the repository:

```sh
git clone https://github.com/edgar-gip/what-where.git
```

Place the directory in `load-path` to make the available to loading, and then
`require` it (this can be done in your `.emacs` file).

```cl
(add-to-list 'load-path "/path/to/what-where")
(require 'what-where)
```

_Integration with Emacs's `package.el` and repositories to simplify installation
is planned._

## Usage

The minor mode can be enabled individually per buffer with `M-x
what-where-mode`, or globally with `M-x global-what-where-mode`. When the mode
is active, invoking the hotkey (`C-c !` by default, customizable using
`what-where-hotkey`) will open a `*What/Where*` buffer in another window with
all _items_ that have been identified by the active _providers_.

Within the `*What/Where*` buffer, the desired item can be selected with `RET`;
that will in turn open a popup to select the _action_ to perform using the
item. Standard actions include to `(C)opy` a suggestion into the kill ring, to
`(R)eplace` the matched span with the suggestion, or to `(F)ind` a suggested
file. The delimited key (`c`, `r`, `f`...) can be used instead of `RET` to
perform the action without going through the popup.

If no _items_ are identified, an informative message is shown (accompanied by a
`beep` if `what-where-beep-on-no-items` is `t`) and the `*What/Where*` buffer is
not shown.

Pressing `DEL`, `C-g` or `q` steps back, either leaving the `*What/Where*`
buffer or returning to the item list after opening the popup on one.

### Preference Learning

If `what-where-ranker-learn` is `t` (the default), `what-where` will train a
simple Machine Learning model (a voted perceptron with margin) based on user
selections. By default, items ranked with a negative score by the model will not
be shown, but pressing `TAB` on the `*What/Where*` buffer will toggle their
visibility.

The model is persisted across sessions in the `what-where-ranker-model-file`,
which by default is `what-where-ranker.dat` file of the `user-emacs-directory`
(`~/.emacs.d/` by default). Saving happens periodically, every
`what-where-ranker-epochs-to-save-model` selections.

Hyper parameters of the learning process can be customized with
`what-where-ranker-margin`, `what-where-learning-rate` and
`what-where-ranker-update-when-none-selected`.

## Built-in Providers

`what-where` comes with two default providers:

*   The `numbers` provider allows conversions between different bases, roman
    numerals, and timestamps. For the latter, `what-where-numbers-timezone` and
    `what-where-numbers-time-format` control the conversion of timestamps to
    date; whereas `what-where-numbers-time-min-year` and
    `what-where-numbers-time-max-year` determine what numbers the provider will
    interpret as a year.
    
*   The `ffap` provider invokes `ffap-file-at-point` and exposes a potential
    match as an item.

## Extension: Custom Providers

Providers are functions that look at `(point)` and call `(what-where-add-item
item)` for each _item_ they have identified. Items are constructed by calling
`what-where-make-item` with the following keyword arguments:

*   `:focus-start`: _(position)_ Start of the text span that triggers the item.

*   `:focus-end`: _(position)_ End of the text span that triggers the item.

*   `:type`: _(string)_ The item _type_, an informative label which is shown in
    `*What/Where*` buffer.
    
*   `:contents`: _(string)_ The item _contents_, also shown in the
    `*What/Where*` buffer but expected to be directly used by the actions.
    
*   `:features`: _(list)_ The item _features_, used by the Machine Learning
    model. It is typically constructed using the `what-where-features` macro,
    which takes a list where each element is:
    
    *    A raw `symbol`, which is interpreted as a binary feature.
    
    *    A `(symbol number)` or `("name" number)` list, which is interpreted
         as a feature with the given value.

    *    A `("template" arguments... number)` list, for which `(format
         "template" arguments...)` is called to generate the feature name.

    Due to the nature of the used Machine Learning model (voted perceptron with
    margin), binary features are recommended.

*   `:actions`: _(list)_ The item actions.

In terms of actions, they are constructed with `what-where-make-action`, which
takes the following arguments:

*   `:shortcut`: _(character)_ The hotkey to invoke the action directly.

*   `:description`: _(string)_ Action description, typically including the
    shortcut character in parentheses.

*   `:function`: _(function)_ Nullary function to invoke when the action is selected.

*   `:feedback`: _(function)_ Nullary function returning a message to show after
    the action has been performed.

*   `:is-terminal-p`: _(boolean, or `early`)_ Whether to close the
    `*What/Where*` buffer _before_ performing the action (`early`), _after_
    (`t`) or not at all (`nil`).

Default actions for copying, replacing, and finding a file can be constructed
with `what-where-copy-action`, `what-where-replace-action`, and
`what-where-find-action`, respectively.

These additional providers can be added to `what-where-providers`, for instance
by making the variable buffer-local as part of a mode-hook:

```cl
(add-hook 'xxx-mode-hook 
          '(lambda ()
              (add-to-list (make-local-variable 'what-where-providers)
                           'custom-provider)))
```
