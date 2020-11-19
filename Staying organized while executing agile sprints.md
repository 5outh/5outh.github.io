---
title: Staying organized while executing agile sprints
date: 2020-11-19
author: Benjamin Kovach
tags:
  - Agile
  - Essays
---

At time of writing, I work as the Scrum Master for a small development team at Freckle Education. When I started the role, I had a hard time keeping on top of all of the little things that come along with the role. I split my time between engineering work myself and facilitating Scrum artifacts, and it used to be pretty common for me to miss a checkin or two each iteration. I've since gotten much better at staying organized, and this post is about how I've done that, primarily using Roam Research as a note-taking tool.

## Background: The Scrum Flavor

At Freckle Education, we run two-week sprints using an adapted version of the LeSS framework. Artifact-wise, I take part in or facilitate the following biweekly discussions:

- Iteration Kickoff
- Story Time
- Planning Poker
- Team Retrospective
- Global (all Scrum Masters) Retrospective

Interspersed in there are some time-bound task- and team-management items that we will discuss in a bit, but this is a good overview for now.

## Background: Roam Research

Roam Research is note-taking tool for organizing thoughts. Pages are created as soon as they are referenced (by a simple notation: just wrap it in double-square-brackets like this `[[New Page]]`, and each day a new daily note for that day is created. When a page is referenced, that page provides a _backlink_ to the page that referenced it. This means that when you reference _future_ dates, when you load the daily note on the app on that day, you will see any other places that you referenced the current day. This is sort of essential to planning out a mostly cyclic schedule. I spend some time setting up the sprint page (_iteration_ is what we call a 2 week sprint at Freckle) and referencing all of the time-bound tasks by their future dates.

If you're not familiar with the pattern, this all sounds super abstract, so I'll move on for now. You may find it useful to revisit this section after seeing an example.

## It starts with a template

I have a page in Roam called `[[Freckle Iteration Template]]`. It has:

- A general "setup" TODO list
- stubs to link to the current iteration project in asana (global and my team's)
- Previous/Next iteration link stubs for easy navigation
- A bunch of relative dates (e.g. "Wednesday A") with a number of subtasks/notes underneath them.

![](https://ben-kovach-blog-assets.s3.amazonaws.com/images/roam-post/iteration-template.png)

When planning for a new iteration, I copy this template to a new page for that (numbered) iteration. For example, `[[Iteration 157]]` is the one I am currently in. I follow the setup steps to make sure the document is in good order. The steps are relatively straightforward, but one of them is crucial: replacing the relative dates with absolute dates is what makes it all work. For example, Iteration 157 started on Wednesday, November 11th, 2020, so I would replace `[[Wednesday A]]` with `[[November 11th, 2020]]` as a part of my setup process. Roam makes this easy with the `/date` command which pulls up a date picker.

Once this setup is done, I will begin to receive backlinks to the dated sections when I open up the program on those days. That helps immensely to keep me on track. It looks kinda like this:

![](https://ben-kovach-blog-assets.s3.amazonaws.com/images/roam-post/backlinks.png)

That's pretty much it for the process. Spend some time setting up a new page at the beginning of the sprint, make sure I check my notes every day (which I do as a habit), and try to tick all the boxes. It works pretty well.

## The nitty-gritty

Apart from the in-app process, the actual iteration template itself deserves some attention. Here's the doc for my current iteration with sections expanded:

![](https://ben-kovach-blog-assets.s3.amazonaws.com/images/roam-post/157-full-iteration.png)

Some things worth noting:

- Metrics and stuff are tracked using the scripts `start-iteration` and `close-iteration`, which are run on the bookends of each iteration.
- After Story Time, I assign out any planned bug fixes to the team before Planning Poker. Bug cost is estimated by individuals and not as a part of Planning Poker; we only focus on user stories there.
- I use planning-poker.com. There is a CSV interface to bulk upload/download stories on that site. I wrote an asana connector to export stories/import costs, which are both marked as TODO items after Story Time.
- We use [stickies.io](https://stickies.io) for the team retrospective. This is a great tool for getting concurrent feedback from everyone and collating it in real-time. I recommend it as a tool.

## Don't want to use Roam?

Roam Research is a great tool, but: 

- it is rather expensive at 15 bucks a month for a personal account
- it does not support local graphs (though you can manually backup your data)
- there have been some historical issues with large-scale data loss
- there have been reports of inappropriate behavior within the company (which I can only hope is being shaken out)

You may find that it is not the tool for you. Luckily, there are other tools out there that do similar things and are adaptable this to type of workflow. [This blog post by Anne-Laure Le Cunff presents some alternatives](https://nesslabs.com/roam-research-alternatives). I have personally tried a few of these tools (Obsidian is the most promising replacement, in my opinion), but none of them quite have the ease-of-use and clean interface that keeps me on Roam. Regardless, the approach can be adapted.

## Wrapping up

I had a hard time finding an organizational system that worked for me when running a small Scrum team. I tried a few other things, and this is what has worked best for me. I hope you can use some of these ideas in your own sprint planning, or qwhatever other cyclical processes you manage.
