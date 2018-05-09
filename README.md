# Ugly Code - Refactor demo

## ScalaSyd Wed 9th May 2018
This is the code base for my ScalaSyd talk on refactoring ugly procedural mutable scala code to be more
idiomatic of Scala.

While there are many parts of this codebase that could do with a tidy up, however
I am just focusing on `provider.api.mapifyHistoricPrices` for the demo.

To use this codebase simply use git to check out the codebase at tag "start" then go to "start_1" then "start_2" then
to "start_n" also aliased to "finish" to see the progression of refactoring

    git co start
    git checkout start_1
    ....
    git checkout start_??

Run the code buy running the Main class at `Main`

The `Main` class needs a `trades.csv` file a list of currencies in `coinlist.csv` and a `historicPrices.csv`.
While you can pass them as arguments to Main it is easier to copy the example files to the default location.

    mkdir -p $HOME/uglycode/coinspotpos
    cp ./exampletrades/* $HOME/uglycode/coinspotpos

now you can run the main class

     sbt clean run

The `Main` class will populate the historic price file from the internet if it is missing some prices. So to see the
prices being fetched delete items from `$HOME/uglycode/coinspotpos/historicPrices.csv` or wipe it

    cat /dev/null >  $HOME/uglycode/coinspotpos/historicPrices.csv

happy re-factoring.