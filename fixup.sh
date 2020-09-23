#!/bin/zsh

mv 1b112336.md "Smart Constructor.md"
find ./ -name "*.md" -exec sed -i -e 's/1b112336/Smart Constructor/g' {} \;
