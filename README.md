# Plain Ledger

### *This project is under construction*

A [KISS](https://en.wikipedia.org/wiki/KISS_principle) plain text double entry
accounting program. No fancy query language or portfolio tracking system. This
program helps you catch mistakes in your accounting text files and exports the
data in a format easy for you to analyse in Calc or Excel. It aims to be a free,
plain text, version control friendly replacement for accounting software like
Sage 50 or QuickBooks (if your needs are simple enough).

All journal files are simple 
[Csv](https://en.wikipedia.org/wiki/Comma-separated_values) files. 
This way you can always edit your financial transactions in the spreadsheet 
program of your choice.

## Available commands
Most commands accept options
to change their behavior (for example specifying the accounting period).

- *transactions* : List all transactions in a csv file.
- *trialbalance* : Prints the trial balance.
- *balancesheet* : Prints the balance sheet.
- *incomestatement* : Prints the income statement.

## Multiple currencies and commodities
Plainledger does *not* provide support for working with multiple currencies.
This is a design choice, it is a [KISS](https://en.wikipedia.org/wiki/KISS_principle)
program that aims to be robust and simple to use. Plus most people and small
businesses do not need multiple currencies anyway.

## License
Plainledger is licensed under the [0BSD](https://opensource.org/licenses/0BSD)
license. Basically, it's yours !

## Related programs

For other full fledged plain text accounting programs you can visit
https://plaintextaccounting.org/.
