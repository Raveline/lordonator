# lordonator

A toy-project for random text generation in Haskell. This is fairly basic stuff
and in need of serious optimization and improvements.

The name comes from Frédéric Lordon, a French economist known for his... how
shall we put it ?... fairly idiosyncratic style. The purpose of this admittedly
silly project is to ultimately build a Twitter bot mimicking Mr. Lordon's rather
unique prose.

Of course, this little application can actually be used to generate any kind of
random texts, provided you have a training set.

## Installation & use

Important note: due to copyright issues, I don't want to put the source material
on the repository. So you'll need to provide your own. By default, building will
expect a "data/" directory, containing a file named "lordon.txt".

Generate random text by using the `lordonator` command, or if you use Stack:

    stack exec lordonator

The process can be rather slow, particularly if your source text is big.

Though there is automatic --help available, here's a quick overview of available
options:

The `--source` option lets you use another source file than the packaged one.
Source file should be flat text.

The `--sentences` option lets you require a given number of sentences. By
default, only one randomish sentence will be generated.

Model training can be set up through `--up` or `-u` for short and `--down` or
`-o` for short. The logic is rather simple : we make a map from n-grams to
a probability for every sequence of words known to follow the n-gram. The
first number is the "n" of "n-gram"; the second the lenght of the sequence of
words. Higher number will yield more coherent result, but have higher chances
of being mere quotation of the source. Lower number yield potential hilarious
results, but with frequent gibberish.

Finally, setting The `--dry` flag won't generate anything, but will print the
training model. (Which tends to be pretty big, so redirecting your output to
a file might be a good idea).

## Does it work ?

Well currently, not really, no. There are tons of issues to fix. But the result
might be funny nonetheless. My todo list includes:

- Reading something serious about Markov algorithm rather than doing this by intuition;

- Improving the normalization of source files, which right now, is kind of a disgrace;

- Finding a way to end sentences in a less abrupt fashion.
