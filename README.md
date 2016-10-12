# sqlite-simple-errors
<a href="https://hackage.haskell.org/package/sqlite-simple-errors">Hackage</a>
<br>
Light wrapper around errors from sqlite-simple to make working with constraint errors a bit easier.

The wrapper function, ```runDBAction``` allows you to run your database code and get the errors returned to you:

```haskell
type DatabaseResponse a = Either SQLiteResponse a
runDBAction :: IO a -> IO (DatabaseResponse a)
```

```SQLiteResponse``` encapsulates the different error types that can be thrown by sqlite-simple, as well as providing a new error type for constraint violations that is more immediately actionable:

```haskell
data Constraint = NotNull
                | ForeignKey
                | Unique
                | Check
  deriving (Show, Eq)

data SQLiteResponse = SQLConstraintError Constraint Text
                    | SQLFormatError FormatError
                    | SQLResultError ResultError
                    | SQLOtherError  SQLError
  deriving (Show, Eq, Typeable)
```

## Usage
Let's use sqlite-simple to create our ```People``` table.

```haskell
createPeopleTableSQL :: Query
createPeopleTableSQL =
  "CREATE TABLE if not exists People (name varchar(50) NOT NULL UNIQUE        \
  \                                  ,age  int         CHECK(age > 0)         \
  \                                  ,id   integer     PRIMARY KEY NOT NULL); "
```

### Not Null Constraint
If we try to insert a record into our database where we provide a ```NULL``` value on a column that is ```NOT NULL```:

```haskell
result <- runDBAction $ execute conn "INSERT INTO People (name, age) VALUES (?, ?)" ( (Nothing, 10) :: (Maybe Text, Int) )
```

where ```result``` is ```Left $ SQLConstraintError NotNull "People.name"```.

The typical error you would get from ```sqlite-simple``` would be a ```SQLError``` whose error context is ```"SQLite3 returned ErrorConstraint while attempting to perform step: NOT NULL constraint failed: People.name"```.

### Unique Constraint
If we try to insert a record into our database that violates a ```UNIQUE``` constraint:

```haskell
_      <- runDBAction $ execute conn "INSERT INTO People (name, age) VALUES (?, ?)" ( ("Joe", 10) :: (Text, Int) )
result <- runDBAction $ execute conn "INSERT INTO People (name, age) VALUES (?, ?)" ( ("Joe", 10) :: (Text, Int) )
```

where ```result``` is ```Left $ SQLConstraintError Unique "People.name"```.

The typical error you would get from ```sqlite-simple``` would be a ```SQLError``` whose error context is ```"SQLite3 returned ErrorConstraint while attempting to perform step: UNIQUE constraint failed: People.name"```.

### Check Constraint
If we try to insert a record that violates some condition we've specified through ```CHECK```:

```haskell
result <- runDBAction $ execute conn "INSERT INTO People (name, age) VALUES (?, ?)" ( ("Joe", 0) :: (Text, Int) )
```

where ```result``` is ```Left $ SQLConstraintError Check "People"```.

The typical error you would get from ```sqlite-simple``` would be a ```SQLError``` whose error context is ```"SQLite3 returned ErrorConstraint while attempting to perform step: CHECK constraint failed: People"```.

### Foreign Key Constraint
Let's make a new table where we can have a foreign key.

```haskell
createJobsTableSQL :: Query
createJobsTableSQL =
  "CREATE TABLE if not exists Jobs (title varchar(50)                              \
  \                                ,person_id int          NOT NULL                \
  \                                ,FOREIGN KEY(person_id) REFERENCES People(id)); "
```

If we try to insert a record into this table without a corresponding entry in the People table:

```haskell
result <- runDBAction $ execute conn "INSERT INTO Jobs (title, person_id) VALUES (?, ?)" (("coder", -1) :: (Text, Int))
```

where ```result``` is ```Left $ SQLConstraintError ForeignKey ""```.

The typical error you would get from ```sqlite-simple``` would be a ```SQLError``` whose error context is ```"SQLite3 returned ErrorConstraint while attempting to perform step: FOREIGN KEY constraint failed"```.
