run:
    sbcl --noinform --non-interactive \
        --eval '(require :asdf)' \
    	--eval '(asdf:load-asd #P"/home/nathan/dev/cl-solitaire/cl-solitaire.asd")' \
        --eval '(asdf:load-system :cl-solitaire)' \
    	--eval '(game-run:run)' \
    	--quit
