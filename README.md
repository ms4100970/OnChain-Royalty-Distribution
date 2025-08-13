# OnChain Royalty Distribution

A Clarity smart contract for automatic royalty distribution to artists and musicians on the Stacks blockchain.

## Overview

This contract enables artists to register themselves and their works, set royalty percentages, and automatically receive payments when their works are purchased. It also supports collaboration between multiple artists on a single work.

## Features

- Artist registration and management
- Work registration with customizable royalty percentages
- Collaborator support with percentage-based revenue sharing
- Automatic royalty distribution on purchase
- Sales tracking and royalty calculation

## Contract Functions

### Administrative Functions

- `set-royalty-percentage`: Set the default royalty percentage (owner only)

### Artist Management

- `register-artist`: Register a new artist
- `update-artist`: Update artist information
- `deactivate-artist`: Deactivate an artist profile

### Work Management

- `register-work`: Register a new work with optional custom royalty percentage
- `update-work-price`: Update the price of a work
- `deactivate-work`: Deactivate a work
- `add-collaborator`: Add a collaborator to a work with a specified share percentage

### Sales and Royalties

- `purchase-work`: Purchase a work, automatically distributing royalties
- `get-artist-royalties`: Calculate total royalties for an artist
- `get-work-royalties`: Calculate royalties for a specific work

### Read-Only Functions

- `get-royalty-percentage`: Get the default royalty percentage
- `get-artist`: Get artist information
- `get-work`: Get work information
- `get-collaborator`: Get collaborator information
- `get-sale`: Get sale information
- `get-artist-works`: Get all active works by an artist

## Usage Example

1. Register as an artist:
   ```
   (contract-call? .onchain-royalties register-artist "Artist Name")
   ```

2. Register a work:
   ```
   (contract-call? .onchain-royalties register-work u1 "Work Title" u1000000 u15)
   ```

3. Purchase a work:
   ```
   (contract-call? .onchain-royalties purchase-work u1)
   ```

4. Add a collaborator:
   ```
   (contract-call? .onchain-royalties add-collaborator u1 u2 u30)
   ```

## Error Codes

- `u100`: Owner only function
- `u101`: Entity not found
- `u102`: Entity already exists
- `u103`: Invalid percentage (must be 0-100)
- `u104`: Insufficient funds
- `u105`: Unauthorized operation
- `u106`: No royalties available
- `u107`: Zero amount not allowed
```
