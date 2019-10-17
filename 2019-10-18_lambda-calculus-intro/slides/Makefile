default: pdf

pdf:
	pandoc \
		--from=markdown+fenced_code_blocks+fenced_code_attributes+tex_math_dollars \
		--to beamer \
		--latex-engine=xelatex \
		--variable fontsize=9pt \
		lambda-calculus.pandoc \
		-o slides.pdf
