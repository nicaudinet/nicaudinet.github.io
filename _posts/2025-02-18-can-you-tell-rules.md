---
layout: post
title: "Can You Tell? - How To Play"
excerpt: The rules for my new LLM-based game!
---

[![title-page.png]({{ site.baseurl }}public/images/can-you-tell/title-page.png){:width="70%" style="display:block; margin-left:auto; margin-right:auto"}](https://canyoutell.eu)

I recently finished the first version of Can You Tell?, a chat-based game with
Large Language Models (LLMs). I'm very excited for you to try it out! You can
find the game at [**canyoutell.eu**](https://canyoutell.eu). I have disabled account
creation for testing. **If you would like an account, contact me directly** and
I will make one for you!

> Note: **Don't write anything important on here!** This game is still in its
> testing phase and is unstable.
{: .block-warning }


# How To Play

Can You Tell? is a game where users take turns to send each other messages:

![chat-example.png]({{ site.baseurl }}public/images/can-you-tell/chat-example.png){:width="70%" style="display:block; margin-left:auto; margin-right:auto"}

Every once in a while, rather than sending a normal message, you will be asked
to write a prompt for a Large Language Model (LLM) instead:

![prompt.png]({{ site.baseurl }}public/images/can-you-tell/prompt.png){:width="70%" style="display:block; margin-left:auto; margin-right:auto"}

The LLM will then write the message for you based on your prompt and previous messages:

![prompt-and-text.png]({{ site.baseurl }}public/images/can-you-tell/prompt-and-text.png){:width="40%" style="display:block; margin-left:auto; margin-right:auto"}

At any time, you or your opponent can make a guess as to whether a message came
from you or an LLM. Guesses are made by clicking on one of the two icons under
messages from your opponent:

![guess-buttons.png]({{ site.baseurl }}public/images/can-you-tell/guess-buttons.png){:width="40%" style="display:block; margin-left:auto; margin-right:auto"}

If a guess is correct, your opponent will lose a star. However, if a guess is
incorrect then you will lose a star instead. **The goal of the game is to make
your opponent run out stars.**

> Note: There are still a few bugs I'm working out. If the game stops working as
> intended for whatever reason, [do a hard
> refresh](https://www.gavel.io/resources/what-is-a-hard-refresh-how-to-do-a-hard-refresh-in-any-browser)
> and log back in. This should fix most issues.
{: .block-warning }


# Starting a game

Once you login, you will be met with the main game page. The column titled
`Chats` contains the current chats with other users.

To start a new game, press the `+ New Game` button on the bottom left:

![new-chat-button.png]({{ site.baseurl }}public/images/can-you-tell/new-chat-button.png){:width="50%" style="display:block; margin-left:auto; margin-right:auto"}

This will bring you to the `Find User` page:

![find-user-empty.png]({{ site.baseurl }}public/images/can-you-tell/find-user-empty.png){:width="50%" style="display:block; margin-left:auto; margin-right:auto"}

To find a user to chat with, type their name (or part of it) in the input field
and press the magnifying class button. This will bring up a list of names that
match:

![find-user-julia.png]({{ site.baseurl }}public/images/can-you-tell/find-user-julia.png){:width="50%" style="display:block; margin-left:auto; margin-right:auto"}

Clicking on a user will will bring you to the `New Chat` page. To initiate a new
chat, write your initial message in the text input at the bottom of the page and
press `Enter` (or click the send button).

You can only play a single game at a time with another user.

# Additional details

The LLM used for the game is `llama-3.3-70b-versatile` provided by
[Groq](https://groq.com/).

Every LLM prompt will also have the following additional system prompt added to
it:

> You are playing a game where you have to imitate what a human would write in a
> chat. You will be given a chat history and a prompt. Write short, human-like
> responses based on the prompt
