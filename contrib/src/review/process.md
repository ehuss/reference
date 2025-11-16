# Review process flowchart

When reviewing a pull request, ask yourself the following questions:

## Are the proposed changes true?

If we're not sure and can't easily verify it ourselves, we ask someone who would know.

## Does this make any new guarantees about the language?

If this would make a new guarantee about the language, this needs to go through the `lang` team to be accepted (unless the `lang` team has clearly accepted this guarantee elsewhere). Ask @traviscross or @pnkfelix if at all unsure about any of these.

## Would we have added this to the Reference ourselves?

There are a number of PRs that might be true, but when we look at them, we think to ourselves, in our heart of hearts, that this just isn't something we would have bothered to write ourselves. We don't want to accept a PR just because it's in front of us and not obviously false. It should clearly add value.

## Is this editorially sound?

Some PRs try to "sell" the language too much, or try to explain more (or less) than needed, or give too many (or too few) examples, etc. The PR should match the general flavor of the Reference here.

## Is this well written?

Some PRs are right but are awkwardly worded or have typographical problems. If the changes are small, we'll just add commits to the branch to clean things up, then merge.

<!-- TODO -->
This policy does not yet cover the process for getting final approval from the relevant teams.
