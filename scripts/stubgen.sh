#!/bin/sh
stubgen -m "$@" -o $(python -c "import $@, os; print(os.path.dirname($@.__file__))")
