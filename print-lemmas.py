from textwrap import dedent
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter

lemma_lines = []
lemmas_following = False
with open('./Veritas/src/test/scala/de/tu_darmstadt/veritas/scalaspl/SQLSpec.scala') as f:
    for line in f:
        if '// LEMMAS END' in line:
            lemmas_following = False
        if lemmas_following:
            lemma_lines.append(line.rstrip())
        if '// LEMMAS BEGIN' in line:
            lemmas_following = True

lemmas = dedent('\n'.join(lemma_lines))
lemmas = lemmas.replace('@Property\n', '')

lexer = get_lexer_by_name("scala", stripall=True)
formatter = HtmlFormatter(linenos=False, cssclass="source", full=True)
result = highlight(lemmas, lexer, formatter)

with open('lemmas.html', 'w') as f:
    f.write(result)
