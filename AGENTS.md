## Learned User Preferences

- Prefer detailed explanations of Elisp and Emacs concepts; user is not advanced at Elisp.
- Org mode is essential for planning, documents, and presentations.
- Prefer Helm for file/buffer/M-x completion; not Consult or Vertico.
- Use Swiper/Counsel for in-buffer search and recent files, but do not enable global `ivy-mode` or `counsel-mode` (conflicts with Helm).
- Prefer SLy over Slime for Common Lisp; keep Elisp and Common Lisp as the only active programming stacks by default.
- Opt-in future language/LSP configs via `lisp/dev/` and `my/dev-modules` in `local.el`.
- Prioritize fast Emacs startup and modular changes that do not break the whole config.
- Test config changes with `my/reload-config-module`, separate Emacs instances, or batch load—not full restarts for every tweak.
- Use Hyper (Fn) key bindings for Emacs-only shortcuts; `C-SPC` does not reach Emacs reliably on this Mac.
- German umlauts via Karabiner fn+letter → Hyper in `lisp/04-german.el` (fn+a→C-M-a→ä, fn+Shift+a→C-M-S-a→Ä); Option-key mappings do not work in Emacs.

## Learned Workspace Facts

- Emacs config lives at `~/.emacs.d` with modular layout: `early-init.el`, slim `init.el` loader, and numbered `lisp/*.el` modules.
- Machine-specific overrides belong in `local.el` (gitignored), modeled on `local.el.example`.
- Opt-in dev modules live under `lisp/dev/`; archived experiments under `lisp/archive/`.
- macOS maps Hyper via `mac-function-modifier` (Fn key); Hyper bindings (H-SPC, H-a, etc.) work where C-SPC does not.
- Use `/Applications/Emacs.app/Contents/MacOS/Emacs` for batch config verification; shell `emacs` may alias to `emacsclient`.
- `use-package` blocks that install/load packages must run after `package-initialize` in `init.el`, not in `01-core.el`.
- Swiper/Ivy/Counsel stack is in `lisp/05-swiper.el` (loaded after `05-helm.el`).
- German umlaut input is in `lisp/04-german.el`.
- Key bindings: C-s→swiper, C-x C-r→counsel-recentf, H-SPC→set-mark-command.
- Relocated bindings for umlaut conflicts: ace-window on F9 a, flyspell hydra on H-f.
