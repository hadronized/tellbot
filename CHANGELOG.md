tellbot CHANGELOG
=================

### 0.6.0.8

- Fixed IRC colors change in HTML title.

### 0.6.0.8

- Fixed IRC colors change in HTML title.

### 0.6.0.7

- Fixed IRC injection in HTML title.

### 0.6.0.6

- Fixed IRC injection in HTML title.

### 0.6.0.5

- Support for bifunctors-5.1.
- Fixed whitespaces issues in tellbot.cabal.
- Fixed useless line in CHANGELOG.md. 

### 0.6.0.4

- Nicer colors for titles.
- Fixed title (whitespaces trimming).

### 0.6.0.3

#### Patch changes

- Added colors!

### 0.6.0.2

#### Patch changes

- Fixed UTF-8 issues with titles.
- Fixed looping on failure (uncaught exception).

### 0.6.0.1

- Fixed CHANGELOG.md.

### 0.6

- Dropped `errors` dependency.

### 0.5.1.4

- fixed HTML entities to correctly decode them
- removed newlines from title

### 0.5.1.3

- added regex list support to filter trusted hosts

### 0.5.1.2

- added HTML in other-modules (doesn’t compile otherwise)

### 0.5.1.1

- patch for old GHC versions (fixes uses of pure)

### 0.5.1

- added an URL parser to detect what it contains

### 0.5

- dependencies updated.

### 0.4.0.3

- bug fix; tells undeletion has been fixed (sorry for bugging, Den!).

### 0.4.0.2

- bug fix; nickname case sensitivity removed.

### 0.4.0.1

- changed the format of *author* and *maintainer* fields in the cabal file;
- general changelog formating;
- added the changelog in the cabal file;
- added source repository information in the cabal file.

### 0.4.0.0

- Notices are now limited to the channel (no need to notice one user).

### 0.3.4.0

- Notices are now available! See the help for further information.

### 0.3.3.2

- Fallback `!tell` behavior – it was a bad idea; now we can use the command
  even if the user is there;
- help in private.

### 0.3.3.1

- Changed credits.

### 0.3.3.0

- It’s now required to set a password when running the bot on a channel; that
  password will be asked when a user attempts to take control of the bot with
  `!do pwd action parameters`; that enables a user to reop, for instance, or
  make the bot say something, or whatever (!help for further information);
- Removed the *!help* output when using `!help`;
- Made the bot a bit less rude ;).

### 0.3.2.0

- When a message is recorded for a specific user, the bot now lets you know
  in private.

### 0.3.1.0

- Tells are now private; the bot still outputs anything else on the channel.

### 0.3.0.0

- It’s now possible to pass the nick of the tellbot on the command line.

### 0.2.2.0

- The bot now only delivers a message at first message after join.

### 0.2.1.0

- The bot rejoins on kick.

### 0.2.0.1

- Added support for IRC server anti-DDoS and netsplit;
- Fixed issues with privileged folks;
- The tellbot will not register stories for itself.

### 0.2.0.0

- Fixed flood attempts;
- Changed `!tell` behavior; now the tellbot will refuse to record your message(s) if
  the recipient is already in the same room as you and the tellbot – the
  recipient has to be out off the room – and it will tell the message(s) when
  the recipient joins.

### 0.1.0.0

- Initial release.
