How to contribute to the the Swirl project?
===========================================

First of all, thank you very much for passing by and for your interest in contributing to the [Swirl project](http://swirl-project.org/). We hope you will find all needed information in this document. If you have any questions, please don't wait to [get in touch with us](#get_in_touch).

People behind the project
-------------------------

The project was started by Dave Cottlehuber [@dch](http://twitter.com/dch). 

Git workflow
------------

We are using github for the whole code base. There are many documents explaining a _github-workflow_ and we are using a similar one. Basically we are differentating between to workflows. One for _contributors_ via a pull request and one for _committers_ with a feature-branch workflow. But first some words on _commit messages_ and _feature branches_.

###General format for commit messages

The git history (or log) is a very important documentation for finding commits and changes in the software. Because of this reason, it is very important to write good commit messages. Please follow these easy rules:

* Write a informative headline not longer than about 60 characters
* If you have been working on a ticket or issue, please use the ticket or issue title and if available the ticket or issue number as the title
* include a newline after the title
* include as many information needed to understand what you have written or changed as needed 

###Feature branches<a name="feature_branches"></a>

If you are working on a bigger part, please create a feature-branch first and work on this branch. The convention we are using for the name is

    feature/name_of_the_branch

The name should be clear and informative. If you are working on a ticket or issue, please use the name of the ticket or issue as the name for the branch.

__Example:__

You want to work on the issue [add getopt support for ./swirl command](https://github.com/skunkwerks/swirl/issues/5). A good name for a feature-branch would be:

    feature/5-add_getopt_support_for_swirl_command

###Contributors workflow

The first step is to fork the project at github to your own account. You are then able to work on the code base. Then create a feature branch like shown in [Feature branches](#feature_branches). After finishing your work, please send a [pull request](https://help.github.com/articles/using-pull-requests) to us. We will then review the code in your pull request, leave comments and finally accept or reject the pull request.

###Committers workflow

As a committer you can directly push to the main repository but we encourage you to use the PR workflow shown above for features other than small typo fixes. We expect a little bit higher standards from committers so please follow this workflow:

1. create a feature branch for bigger changes or tasks or tickets. If it is a ticket the feature branch should be named like: 1912-troubleshooting-guide where 1912 is the ticket number and "Troubleshooting guide" the title of the ticket
2. please follow these steps for your work:
    - create feature branch from master
    - work on feature branch
    - clean up feature branch commits (clean up whitespace in the code area)
    - rebase feature branch onto current master
    - merge feature branch into master
3. rebase your work into logical steps - this results in the following single commits
    - minimally complete refactoring #1
    - minimally complete refactoring #2
    - minimally complete refactoring #3
    - actual feature/fix, part #1
    - actual feature/fix, part #2

Documentation
-------------

One important documentation bit is the [Peer-to-Peer Streaming Peer Protocol (PPSPP) draft-ietf-ppsp-peer-protocol-09](http://tools.ietf.org/html/draft-ietf-ppsp-peer-protocol-09) (work in progress). The idea behind Swirl is based on this protocol draft.

The documentation for the project itself is recently still work in progress. You can find the first written documentation files within the gut branch [feature/docs](https://github.com/skunkwerks/swirl/tree/feature/docs).

Get in touch<a name="get_in_touch"></a>
------------

###Mailinglist

The Swirl project is using [librelist](http://librelist.com). You can subcribe your email address by simply sending an email to swirl@librelist.com. You will then receive an email with the subject "Confirmation required". Please reply this message. With receiving a confirmation email, including some important information, you will be subscribed.

###IRC

You can join the project members via IRC. The channel is called _#swirl_ and is hosted at _irc.freenode.net_. There are various [IRC clients](http://en.wikipedia.org/wiki/Comparison_of_Internet_Relay_Chat_clients) you can use in your operating system.

Testing
-------

The code of the project is tested. So when sending a pull request from your fork or when adding code as a contributor, please add appropriate tests. A good start is to have a lok into the [test dirctory](https://github.com/skunkwerks/swirl/tree/master/test).

Bug reports
-----------

The project is using the issue tracker in it's github page. You can find it at [https://github.com/skunkwerks/swirl/issues](https://github.com/skunkwerks/swirl/issues). If you think you found a bug, please have a look into the issues list first. If you can't find a similar issue, you are welcome to open a new one.

Become a contributor
--------------------

We are very happy to welcome new contributors to the project. To become a committer, we ask you to fork this project and send a pull request with your contributions. By doing so in a regular basis, we are eventually happy to add you as a contributor.

Please make sure, that you put all the code you write under the [Apache 2 License](http://www.apache.org/licenses/LICENSE-2.0.html).
