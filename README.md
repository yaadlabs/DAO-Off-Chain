# triphut-dao-offchain

## Getting Started

First, enter the Nix shell by running `nix develop`.

## Testing

To execute the Plutip tests, run `npm run test`.

## Building

The project can be built by running `spago build`.

## Updating On-Chain Scripts

If scripts have been updated in the `src/Dao/Scripts/Json` directory, you will
need to run `make generate-purs-scripts` in order to copy these into PureScript.

## Using as an NPM Package Locally

This library will be made available as an NPM package. However, if you wish to
make changes to this library and test those immediately in another project, you
can create a local NPM package that will update after any build. Simply follow
the steps below:

1. Open a new shell and navigate to the project root (do not enter Nix shell).
2. Run `npm link`. 
3. Navigate to the project you wish to import this library into and run 
   `npm link triphut-dao-offchain`. You will now be able to import the local 
   library as if installed from NPM.
