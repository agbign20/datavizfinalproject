## March 23 Beginning of Class

I found a data set for my final project containing a MLB data set. I loaded in the data set and planned out what I wanted in the app on a separate piece of paper. Then I started creating the shiny app using the MLB data set.


## March 23 End of Class

I continued creating the shiny app for the MLB data set. I also have problems with reordering the players according to their salary. I attempted to fix this while working on my project.


## March 28 Beginning of Class

I played around with the tab panel layout on my shiny app and in the end decided to leave it in intervals of 1 year with the last tab allowing users to customize the years they want to look at.


## March 28 End of Class

I tried fixing the year filter model for the customized years. I also tried seeing if there is a way to extend the shiny app window in order to see the app more clearly, but decided that the tabset panel was better than the navlist panel.


## March 30 Beginning of Class

I started creating another graph with the position of the team with the number of players in that position. It will not show in the shiny app right now. I also tried working on the customized years tab panel but I still couldn't fix it.


## March 30 End of Class

I tried fixing problems with GitHub I had. I also fixed the error I had with my histogram slightly, even though I am still having problems with it trying to show up in my shiny app and factoring it.


## April 4 Beginning of Class

I tried fixing my graph with the positions and number of players through a static graph to see if there is something wrong with the graph in the first place, which there is. Also started trying to create a line in the first graph with the avg salary of those players.


## April 4 End of Class

I figured out what was wrong with the positions graph through a static graph. I had problems with the Shiny app with the positions graph, but I also fixed that.


## April 6 Beginning of Class

I realized that I wanted the positions graph to graph the positions depending on the team chosen by the user. I eventually fixed that by grouping team too in the data frame. But now there is a factoring problem in the positions graph.


## April 6 End of Class

There is still a factoring problem, but finished making a line in the first graph with the avg salary of those players on the team chosen. I also formatted the page with a title for the app in the sidebar panel.


## April 11 Beginning of Class

I restarted my project in a separate way because there was an excessive amount of code. While I was rewriting the code, I decided to get rid of the tab set panel of each year to fill the sidebar.


## April 13 Beginning of Class

I changed the input for the years to a selectizeInput. Then I created two more inputs, TopSlice and BottomSlice, which creates a slicing components for users to decided when creating the salary graph. I also tried looking at scale continuous for the x and y axis for the graphs but it made the graph look worse so I decided against it.


## April 18 Beginning of Class

I tried figuring out why the graphs were not showing up when I selected different teams. I also added the multiple option for the teams, but also could not figure out why the geom_bar is not showing the correct graph of the positions data set.


## April 18 End of Class

I fixed the geom_bar so the dodge position shows how many players are in each position. I also added the fill = Team for both graphs so that when the user chooses multiple teams, they can see which team has higher salary or more players in that position.


## April 20 Beginning of Class

I tried fixing the slicing problems I am having for the salary because the ordering does not seem to be correct. It seems to only get remove the other teams that I add to the input. Then I changed the colors to a brewer palette. I tried creating an input for users to choose a theme but also ran into problems.

