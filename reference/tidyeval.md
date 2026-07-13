# Tidy eval helpers

The `.data` pronoun is reexported from rlang. It represents the current
slice of data inside data-masking verbs; if you have a column name
stored in a string, use `.data[["var"]]` to refer to that column. See
the [rlang reference](https://rlang.r-lib.org/reference/dot-data.html)
for details.
