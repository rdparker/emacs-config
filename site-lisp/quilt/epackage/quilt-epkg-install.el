(require 'quilt-epkg-autoloads)
(add-hook 'find-file-hooks 'quilt-hook)
(add-hook 'after-revert-hook 'quilt-hook)
(provide 'quilt-epkg-install)
