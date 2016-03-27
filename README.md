# splash-buffer.el

`splash-buffer.el` is a package for creating splash/startup screens for Emacs,
similar to the `*GNU Emacs*` or `*spacemacs*` buffers that many users are
familiar with. It provides the `splash-buffer-define` macro for this purpose.

Although designed for creating custom homepage-like buffers, the package can
really be used to create any kind of buffer containing pre-defined content. For
example, the following will create an interactive function `scratch-buffer` that
creates/switches to something identical to the `*scratch*` buffer:

```elisp
(splash-buffer-define scratch-buffer "*scratch-1*"
  :mode lisp-interaction-mode
  (insert (substitute-command-keys initial-scratch-message)))
```

This project is in its early stages, and its design may change substantially
without warning.
