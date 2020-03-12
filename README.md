# Plain Ledger

### *This project is under construction*

A [KISS](https://en.wikipedia.org/wiki/KISS_principle) plain text double entry
accounting program. No fancy query language or portfolio tracking system. This
program helps you catch mistakes in your accounting text files and exports the
data in a format easy for you to analyse in Calc or Excel. It aims to be a free,
plain text, version control friendly replacement for accounting software like
Sage 50 or QuickBooks (if your needs are simple enough).

The journal file containing all financial information is written in
[Yaml](https://yaml.org/), a popular format easy to read and modify by hand or
with any programming language. You can also write your financial information in a
[Csv](https://en.wikipedia.org/wiki/Comma-separated_values) file and link to
those files in your journal file.

## Available commands
Most commands accept options
to change their behavior (for example specifying the accounting period).

- *accounts* : List all accounts and their properties.
- *transactions* : List all transactions in a csv file.
- *trialbalance* : Prints the trial balance.
- *convert* : Convert between the various formats (CSV, Yaml) understood by plainledger.

## Multiple currencies and commodities
Plainledger provides very basic support for working with multiple currencies.
You can enter transactions with any commodity you like and plainledger will not
mix them up. For example, earnings in balance sheet report are computed by
commodity. But plainledger does automatically convert amounts in a foreign
currency to your local currency or compute realized and unrealized foreign
exchange gain. Feel free to open an issue if you need such things.

## ToDo
- [ ] Improve documentation

## License
Plainledger is licensed under the [0BSD](https://opensource.org/licenses/0BSD)
license. Basically, it's yours !

## Related programs

For other full fledged plain text accounting programs you can visit
https://plaintextaccounting.org/.
