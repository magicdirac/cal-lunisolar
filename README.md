## Introduction

`cal-lunisolar` is a GNU Emacs package for the Traditional East Asian lunisolar calendar. Emacs version of localization for China, Korea, Vietnam, and Japan and countries in the East Asian cultural sphere. — Edit

The package is still under-develepment. 

## Configuration overview

### add path of this package to your `load-path
```elisp
(add-to-list 'load-path
	     "path/to/the/directory/of/cal-lunisolar")
```
then simply `require` the package
```elisp
(require 'cal-lunisolar)
```

### A few customizations are provided.

#### Control wheather showing lunar date or celestial-terrestrial date
> show lunar date 
```elisp
(setq cal-lunisolar-display-lunar t)  ;; show lunar date in the buffer.
```
> or show celestial-terrestrial date

```elisp
(setq cal-lunisolar-display-lunar nil)  ;; show celestial-terrestrial date in the buffer.
```

#### aligning CJK and ASCII fonts

For aligning CJK and ASCII font, [chinese-fonts-setup](https://github.com/tumashu/chinese-fonts-setup) may be helpful.

> If you have  installed [chinese-fonts-setup](https://github.com/tumashu/chinese-fonts-setup), 
```elisp
(setq cal-lunisolar-force-align nil)  ;; default 
```
> If you have not installed [chinese-fonts-setup](https://github.com/tumashu/chinese-fonts-setup), you can set `cal-lunisolar-force-align` to `t` which will try to align the date entries. Currently I have assumed there are only 2 CJK characters in each date entry.
```elisp
(setq cal-lunisolar-force-align t)
```

## Contributing

### Copyright Assignment

`cal-lunisolar` is subject to the same [copyright assignment](http://www.gnu.org/prep/maintain/html_node/Copyright-Papers.html) policy as Emacs itself, org-mode, CEDET and other packages in [GNU ELPA](http://elpa.gnu.org/packages/). Any [legally significant](http://www.gnu.org/prep/maintain/html_node/Legally-Significant.html#Legally-Significant) contributions can only be accepted after the author has completed their paperwork. Please see [the request form](http://git.savannah.gnu.org/cgit/gnulib.git/tree/doc/Copyright/request-assign.future) if you want to proceed.

The copyright assignment isn't a big deal, it just says that the copyright for your submitted changes to Emacs belongs to the FSF. This assignment works for all projects related to Emacs. To obtain it, you need to send one email, then send one letter (if you live in the US, it's digital), and wait for some time (in my case, I had to wait for one month).

### Style

Use your own judgment for the commit messages, I recommend a verbose style using `magit-commit-add-log`.
