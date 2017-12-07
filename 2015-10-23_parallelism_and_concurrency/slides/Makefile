default: revealjs pdf

revealjs:
	pandoc \
		--standalone \
		--from=markdown+fenced_code_blocks+fenced_code_attributes \
		--to revealjs \
		--template=tng-template.html \
		--no-highlight \
		--variable hlss=github \
		parallel-concurrent.pandoc \
		-o reveal.js/index.html

	# convert h1 to h2 in level2 slides
	perl -0777 -p -i -e 's#(.*?level2">\s*\n)<h1>(.*?)</h1>#$$1<h2>$$2</h2>#sg' reveal.js/index.html

pdf:
	pandoc \
		--from=markdown+fenced_code_blocks+fenced_code_attributes \
		--to beamer \
		--latex-engine=xelatex \
		--variable fontsize=9pt \
		parallel-concurrent.pandoc \
		-o out.pdf

clean:
	rm reveal.js/index.html out.pdf
