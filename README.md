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
with any programming language.

Plainledger sees accounting as isomorphic to a directed graph, where accounts
are nodes and transactions edges. This means that, like
[Transity](https://github.com/feramhq/transity) transactions are entered by
specifying that an amount was transferred from account A to account B instead of
the confusing debit and credit style. But plainledger can export financial
reports in a debit and credit format that your accountant will love.

## Available commands
Unless specified, all outputs are in CSV format. Most commands accept options
to change their behavior (for example specifying the accounting period).

- *Accounts* : List all accounts and their properties.
- *Transfers* : List all transfers in a From-To style.
- *Transactions* : List all transfers in the standard accounting
  style (Debit-Credit). Not implemented yet.
- *Balance sheet* : Prints the balance sheet. Not implemented yet.
- *Income statement* : Prints the income statement. Not implemented yet.
- *Trial balance* : Prints the trial balance. Not implemented yet.
- *From CSV* : Convert a CSV file into a Yaml file readable by plainledger.
  Use this command to batch modify (or import) transactions, accounts or balance assertions
  in Calc or Excel and then propagate back those changes to the Yaml journal file.

## ToDo
- [ ] Implement the above commands.
- [ ] Better documentation on the accounting and directed graph isomorphism.
- [ ] Better documentation on how to use the software with multiple currencies.
- [ ] Better documentation on how to use the convert command to import transactions.

## License
Plainledger is licensed under the [0BSD](https://opensource.org/licenses/0BSD)
license. Basically, it's yours !

## Related programs

For other full fledged plain text accounting programs you can visit
https://plaintextaccounting.org/.
