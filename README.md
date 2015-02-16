# Mars

Mars allows you to explore a JSON data structure using commands similar to the
ones used for navigating UNIX file systems

## Usage

To start a `mars` session

```
$ mars <jsonfile>
```

### Commands

```
pwd
```

Print the current path

```
ls [query]
```

List the keys at the current path or at the, optionally, provided query. If a
query is provided it is evaluated from the current path.

```
cat [query]
```

Print the subtree at the present path.

```
cd <query>
```

Move to the path specified by the query
