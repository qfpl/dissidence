# Dissidence : Compositional Crusaders

This code is mostly an education tool to show a non trivial Elm + Haskell application in a fun setting. The aim is to 
show that fullstack functional web dev is both practical and powerful. 

TODO: Add screenshots later and tidy up words to make this easier to understand.

This is a simple social game where there are two competing factions:

- Compositonal Crusaders: Whose aim is to improve their projects by voting to write their projects using functional 
  programming.
- Sneaky Side-Effects: Whose aim is to ultimately vote against this move to functional technology. 

The game consists of up to five projects. Each project has a certain number of programmers that need to be on it. Each
project has a secret ballot where it must come to a unanimous agreement to use FP for that project. If there is agreement, 
the Crusaders score a point. If there was not an agreement, then the side-effects score a point. First team to hit three
points, wins!

The trick this game is that the team is proposed by a rotating role called a "team-lead" wanting to form a project. That
team composition need not actually include the team lead but must win a public majority vote to be approved. If approved,
the round progresses to the secret ballot to approve it as an FP project or not. If the team was not approved, then the 
team-lead role moves clockwise around the table and we retry.

Any player may say anything during the game so long as it is public. The aim of the crusaders game is to deduce who the 
sabotuers are and avoid putting them on projects so that the projects can succeed. The side-effects need to torpedo the
move to FP without giving themselves away so they are not excluded from future projects.

There are two special roles, one for each faction:
- FP Expert: This person is part of the crusaders and knows FP enough that they can tell who the sabotuers are.
- The Pointy Haired Boss: Part of the side-effects. At the end of the game if the crusaders have won, this person has one
  chance to fire a member of the team. If that person is the FP expert, the side-effects win and FP is lost forever. *sadface*

In this sense, it's important for the expert to not give themselves away. 


*This obviously supposed to be super satirical and not a reflection of real life in any way. We very much hope that your 
actual work and FP transformation is much more friendly/positive and less adversarial/sneaky.* :joy:

This may remind you of a very awesome board game that we have avoided references to so that we don't tread on their 
copyrights and so no one actually plays this instead of buying the real game. It's much more difficult and fun when you 
can look someone in the eyes anyway. 