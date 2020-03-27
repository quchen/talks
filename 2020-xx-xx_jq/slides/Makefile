default: pdf

pdf:
	pandoc \
		--from=markdown+fenced_code_blocks+fenced_code_attributes \
		--to beamer \
		--latex-engine=xelatex \
		--variable fontsize=9pt \
		source.pandoc \
		-o slides.pdf
