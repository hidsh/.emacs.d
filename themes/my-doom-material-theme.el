;;; doom-material-theme.el --- Something like material dark theme  -*- no-byte-compile: t; -*-

(require 'doom-themes)

;;; Commentary:

;; Pale color a little
;; Inspired by Material Theme by equinusocio, thanks!

;;; Code:

(defgroup my-doom-material-theme nil
  "Options for doom-themes."
  :group 'doom-themes)

(defcustom doom-material-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-material-theme
  :type '(choice integer boolean))

;;
(def-doom-theme my-doom-material
  "A dark theme inspired by Material Theme by equinusocio"

  ;; name        default   256       16
  ;; ((bg         '("#263238" nil       nil))
  ((bg         '("#212B31" nil       nil))
   (bg-alt     '("#1C262B" nil       nil))
   (base0      '("#171F24" nil       nil))
   (base1      '("#232B31" "#262626" "brightblack"))    ;; from title bar at dark mode of Mac
   (base2      '("#222D32" "#303030" "brightblack"))
   (base3      '("#171F24" "#3a3a3a" "brightblack"))
   (base4      '("#314048" "#444444" "brightblack"))
   (base5      '("#37474F" "#585858" "brightblack"))
   (base6      '("#556369" "#626262" "brightblack"))
   (base7      '("#737E84" "#767676" "brightblack"))
   (base8      '("#9BA3A7" "#a8a8a8" "white"))
   ;; (fg         '("#EEFFFF" "#e4e4e4" "brightwhite"))
   ;; (fg         '("#FFFBEB" "#e4e4e4" "brightwhite"))
   ;; (fg         '("#FFFEFA" "#e4e4e4" "brightwhite"))
   ;; (fg         '("#FEFFFA" "#e4e4e4" "brightwhite"))
   (fg         '("#F0F9F8" "#e4e4e4" "brightwhite"))
   (fg-alt     '("#BFC7D5" "#bcbcbc" "white"))

   (grey base5)

   (black       '("#000000" nil nil))
   ;; (red         '("#ff5370" "#ff0000" "red"))               ;; orig
   ;; (red         '("#FF8599" "#ffd700" "brightyellow"))
   ;; (red         '("#FF99AA" "#ffd700" "brightyellow"))
   ;; (red         '("#FFADBB" "#ffd700" "brightyellow"))
   (red         '("#FFC2CC" "#ffd700" "red"))
   ;; (dark-red    '("#CC1436" "brown"   "brown"))
   (dark-red    '("#7A0014" "brown"   "brown"))
   ;; (orange      '("#f78c6c" "#ff5f00" "brightred"))     ;; orig
   (orange      '("#FBB86C" "#ffd700" "orange"))
   (dark-orange '("DarkOrange3" "orange3" "orange3"))

   ;; (orange      '("#FFD485" "#ffd700" "brightyellow"))
   ;; (yellow      '("#FFF3C2" "#ffd700" "brightyellow"))
   ;; (yellow      '("#FFEFAD" "#ffd700" "brightyellow"))
   (yellow      '("#FFF3C2" "#ffd700" "brightyellow"))
   (dark-yellow '("#A28915" "yellow3" "yellow3"))
   (dark-yellow2 '("#7E6A10" "yellow3" "yellow3"))
   (dark-yellow3 '("#6C5F0F" "yellow3" "yellow3"))
   ;; (yellow      '("#FFEB99" "#ffd700" "brightyellow"))
   ;; (yellow      '("#FFE785" "#ffd700" "brightyellow"))
   ;; (green       '("#7ECCC4" "#afff00" "green"))
   ;; (green       '("#c3e88d" "#afff00" "green"))
   (green       '("#D3EEAA" "#afff00" "green"))
   (teal           '("#A7DCD8" "#afff00"   "green"))
   ;; (dark-teal      '("#429EA6" "dark cyan" "dark cyan"))
   ;; (dark-teal      '("#408C83" "dark cyan" "dark cyan"))
   ;; (dark-teal      '("#3A928B" "dark cyan" "dark cyan"))
   ;; (dark-teal      '("#2F756F" "dark cyan" "dark cyan"))
   (dark-teal      '("#235853" "dark cyan" "dark cyan"))
   (light-teal     '("#B6E2DE" "#afff00" "green"))
   ;; (blue        '("#82aaff" "#5fafff" "brightblue"))
   ;; (blue        '("#99B9FF" "#5fafff" "brightblue"))
   ;; (blue        '("#A7DCD7" "#afff00" "green"))
   ;; (blue        '("#ADC7FF" "#5fafff" "brightblue"))
   (blue        '("#C2D5FF" "#5fafff" "brightblue"))
   ;; (blue        '("#D6E3FF" "#5fafff" "brightblue"))
   (dark-blue   '("#7986E7" "#d7ffff" "blue"))
   ;; (magenta     '("#c792ea" "#d787d7" "brightmagenta"))
   ;; (magenta     '("#D3A9EF" "#d787d7" "brightmagenta"))
   (magenta     '("#DCBAF2" "#d787d7" "brightmagenta"))
   ;; (violet      '("#bb80b3" "#d787af" "magenta"))
   ;; (violet      '("#C695BF" "#d787af" "magenta"))
   (violet      '("#CDA2C7" "#d787af" "magenta"))
   ;; (violet      '("#D4AFCF" "#d787af" "magenta"))
   ;; (cyan        '("#89DDFF" "#5fd7ff" "brightcyan"))
   ;; (cyan        '("#ADE8FF" "#5fd7ff" "brightcyan"))
   (cyan        '("#C2EEFF" "#5fd7ff" "brightcyan"))
   (dark-cyan   '("#80cbc4" "#00d7af" "cyan"))

   ;; face categories -- required for all themes
   (highlight      teal)
   (highlight-dark (doom-darken highlight 0.3))
   (highlight-red  red)
   (vertical-bar   base2)
;; (selection      base4)
   (selection      nil)
   (builtin        blue)
;; (comments       base6)
   (comments       base8)
;; (doc-comments   base6)
   (doc-comments   base8)
   (constants      red)
   (functions      blue)
   (keywords       cyan)
   (methods        blue)
   (operators      cyan)
   (type           magenta)
   (strings        green)
   (variables      yellow)
   (numbers        yellow)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
;; (modeline-fg     base8)
   (modeline-fg     fg-alt)
   ;; (modeline-bg     base4)
   (modeline-bg     base1)

   (modeline-fg-alt base7)
;; (modeline-bg-alt (doom-darken bg 0.01))
   (modeline-bg-alt bg)

   (-modeline-pad
    (when doom-material-padded-modeline
      (if (integerp doom-material-padded-modeline) doom-material-padded-modeline 4))))

  ;; --- base faces ------------------------
  ;; (((lazy-highlight &override) :background base4 :foreground fg :distant-foreground fg :bold bold)
  ((lazy-highlight :background base6 :foreground 'unspecified :distant-foreground 'unspecified :bold 'unspecified)
   (doom-modeline-buffer-path       :foreground green :weight 'bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)

   ;; highlight-thing highlight-symbol
   (highlight-symbol-face :background region :distant-foreground fg-alt)

   ;; highlight-thing
   (highlight-thing :background region :distant-foreground fg-alt)

   ;; highlight
   (highlight :background "#606060" :foreground "#ffffff" :weight 'bold)

   ;; symbol-overlay
   (symbol-overlay-default-face :background base6 :foreground "#ffffff")
   (symbol-overlay-face-1 :inherit 'symbol-overlay-default-face :background dark-red)
   (symbol-overlay-face-2 :inherit 'symbol-overlay-face-1)
   (symbol-overlay-face-3 :inherit 'symbol-overlay-face-1)
   (symbol-overlay-face-4 :inherit 'symbol-overlay-face-1)
   (symbol-overlay-face-5 :inherit 'symbol-overlay-face-1)
   (symbol-overlay-face-6 :inherit 'symbol-overlay-face-1)
   (symbol-overlay-face-7 :inherit 'symbol-overlay-face-1)
   (symbol-overlay-face-8 :inherit 'symbol-overlay-face-1)

   ;; minibuffer
   (completions-annotations :foreground doc-comments)

   ;; modeline
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))

   (mode-line-active
    :inherit 'mode-line :foreground teal :overline teal)

   (mode-line-inactive
    :inherit 'mode-line :foreground modeline-fg-alt :overline modeline-fg-alt)

   (cursor :foreground black :background teal)
   (cursor-reverse :foreground teal :background black)
;; (scroll-bar :background modeline-bg)
   (vertical-border :foreground highlight)

   (line-number :foreground base7 :background base3)
   (line-number-current-line :foreground teal :background base3)

;; (fringe :background base2)
   ;; (fringe :background base3)
   (fringe :foreground base7 :background base3)

   ;; selection (region)
   (region :foreground black :background teal)

   (isearch :inherit 'lazy-highlight :foreground 'unspecified :background highlight-dark)

   ;; font-lock
   (font-lock-keyword-face :foreground yellow)
   (font-lock-variable-name-face :foreground cyan)

   ;; --- major-mode faces ------------------------
   ;; man-mode
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)

   ;; org-mode
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)

   ;; paren
   ;; (show-paren-match :background black)
   ;; (show-paren-match :background "#3A537E")
   (show-paren-match :background dark-teal :weight 'bold)
   ;; (paren-face-match :foreground nil :background base0)
   (paren-face-match :foreground 'unspecified :background dark-teal)
   (sp-show-pair-match-face :background dark-teal)

   ;; --- plugin faces -------------------
   ;; emacs
   (match :foreground orange :weight 'bold)

   ;; evil
   (my-evil-normal-tag-face :foreground black :background teal)
   (my-evil-emacs-tag-face :inherit 'my-evil-normal-tag-face :background yellow)
   (my-evil-insert-tag-face :inherit 'my-evil-normal-tag-face :background red)
   (my-evil-motion-tag-face :inherit 'my-evil-normal-tag-face :background base7)
   (my-evil-visual-tag-face :inherit 'my-evil-normal-tag-face :background green)
   (my-evil-operator-tag-face :inherit 'my-evil-normal-tag-face :background violet)

   ;; git-gutter
   (git-gutter:modified :background yellow :foreground 'unspecified)
   (git-gutter:added    :background teal   :foreground 'unspecified)
   (git-gutter:deleted  :background red    :foreground 'unspecified)

   (git-gutter-fr:modified :foreground 'unspecified :background dark-yellow)
   (git-gutter-fr:added    :foreground 'unspecified :background dark-teal)
   (git-gutter-fr:deleted  :foreground 'unspecified :background dark-red)

   ;; lsp
   (lsp-face-highlight-read :underline teal)
   (lsp-face-highlight-textual :underline orange)
   (lsp-face-highlight-write :underline red)

   ;; lsp-ui
   (lsp-ui-peek-highlight :foreground black :background orange)

   ;; eglot
   (eglot-diagnostic-tag-deprecated-face :underline orange)
   (eglot-diagnostic-tag-unnecessary-face :underline yellow)

   ;; flycheck (fringe)
   (flycheck-fringe-info    :background 'unspecified :foreground teal)
   (flycheck-fringe-warning :background 'unspecified :foreground yellow)
   (flycheck-fringe-error   :background 'unspecified :foreground red)

   ;; flymake (fringe)
   (compilation-info    :background 'unspecified :foreground teal)
   (compilation-warning :background 'unspecified :foreground yellow)
   (compilation-error   :background 'unspecified :foreground red)

   (flymake-note    :background 'unspecified :foreground teal)
   (flymake-warning :background 'unspecified :foreground yellow)
   (flymake-error   :background 'unspecified :foreground red)

   ;; css-mode / scss-mode
   (css-proprietary-property :foreground yellow)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; web-mode
   ((web-mode-current-element-highlight-face &override) :underline highlight)

   ;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored :foreground cyan)
   (dired-k-added    :foreground vc-added)

   ;; vertico
   (vertico-group-separator :foreground red)
   ;; (vertico-current :foreground teal :background black :inverse-video t)
   (vertico-current :underline teal :background black)
   ;; (vertico-group-title :foreground orange :inverse-video t)
   (vertico-group-title :foreground base3 :background base8)
   (vertico-multiline :foreground base8)

   ;; orderless
   (orderless-match-face-0 :inherit 'match)                         ;; for recentf and others
   (orderless-match-face-3 :foreground "#72a4ff" :weight 'bold)     ;; use color of orderless-match-face-0

   ;; consult
   (consult-preview-match :inherit 'match)                 ;; consult-grep
   (consult-separator :foreground base8 :background red)
   (consult-grep-context :foreground base8 :background yellow)
   (consult-help :foreground base8 :background green)
   (consult-preview-insertion :inherit 'vertico-current)

   ;; ivy
   ;; (set-face-foreground 'ivy-action (mycolor 'red))
   ;; (set-face-background 'ivy-confirm-face "'green")
   ;; (set-face-attribute 'ivy-minibuffer-match-face-1 nil :foreground nil :background nil :bold t :underline t)
   ;; (copy-face 'ivy-minibuffer-match-face-1 'ivy-minibuffer-match-face-2)
   ;; (copy-face 'ivy-minibuffer-match-face-1 'ivy-minibuffer-match-face-3)
   ;; (copy-face 'ivy-minibuffer-match-face-1 'ivy-minibuffer-match-face-4)
   (ivy-action :foreground red)
   (ivy-confirm-face :foreground green)
   (ivy-current-match :background base5)
   ;; (ivy-cursor )
   ;; (ivy-highlight-face )
   (ivy-match-required-face :foreground red)
   (ivy-minibuffer-match-face-1 :background highlight :underline highlight)
   (ivy-minibuffer-match-face-2 :underline highlight)
   (ivy-minibuffer-match-face-3 :underline highlight)
   (ivy-minibuffer-match-face-4 :underline highlight)
   (ivy-minibuffer-match-highlight :underline highlight)
   (ivy-modified-buffer :foreground red)
   ;; (ivy-prompt-match )
   (ivy-remote :foreground red)
   (ivy-subdir :foreground highlight)
   (ivy-virtual :foreground yellow)

   ;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground yellow)
   (js2-object-property-access :foreground cyan)
   (js2-function-param         :foreground violet)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :weight 'ultra-bold :foreground "#FFFFFF")
   (rainbow-delimiters-depth-2-face :weight 'bold :foreground blue)
   (rainbow-delimiters-depth-3-face :weight 'bold :foreground green)
   (rainbow-delimiters-depth-4-face :weight 'bold :foreground orange)
   (rainbow-delimiters-depth-5-face :weight 'bold :foreground yellow)
   (rainbow-delimiters-depth-6-face :weight 'bold :foreground red)
   (rainbow-delimiters-depth-7-face :weight 'bold :foreground cyan)
   (rainbow-delimiters-depth-8-face :weight 'bold :foreground violet)
   (rainbow-delimiters-depth-9-face :weight 'bold :foreground fg-alt)

   ;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)

   ;; tabbar
   (tabbar-default :foreground teal :background base1 :underline teal :overline teal)
   (tabbar-selected          :inherit 'tabbar-default :foreground bg-alt :background teal)
   (tabbar-unselected        :inherit 'tabbar-default)
   (tabbar-selected-modified :inherit 'tabbar-selected)
   (tabbar-modified          :inherit 'tabbar-default)
   (tabbar-highlight         :inherit 'tabbar-default)
   (tabbar-separator         :background teal)

   ;; ivy, counsel
   (ivy-current-match :inherit 'cursor :foreground bg)

   ;; corfu
   (corfu-default       :background base5)
   (corfu-current       :inverse-video t :foreground teal)
   (corfu-border        :background teal)
   (corfu-bar           :background teal)
   (corfu-popupinfo     :background base5)
   (corfu-deprecated    :background "#600" :foreground orange)

   ;; company
   ;; (company-tooltip-selection :foreground modeline-bg :background highlight)
   ;; (company-tooltip :foreground "#e4e4e4":background modeline-bg)
   ;; (company-scrollbar-bg :background modeline-bg)

   ;; popper
   (popper-echo-area :foreground modeline-bg :background teal )
   (popper-echo-area-buried :inherit 'popper-echo-area :inverse-video t)
   (popper-echo-dispatch-hint :inherit 'popper-echo-area)

   ;; beacon
   (beacon-color :inherit 'cursor)

   (trailing-whitespace :background error)

   ;; tooltip
   (tooltip              :background (doom-darken bg-alt 0.2) :foreground fg)

   ;; hide-ifdef-mode
   (hide-ifdef-shadow :foreground comments)

   ;; ediff
   (ediff-current-diff-A :background 'unspecified)
   (ediff-fine-diff-A :background 'unspecified)

   ))




(provide 'doom-material-theme)
;;; my-doom-material-theme.el ends here
