This script was developed a few years back when I thought I’d figure out a way to partition a `networkD3` graph interactively in `R` via a `Shiny` interface. At the time – we are talking 2019 - there were not many resources, to my knowledge, that accomplished that. (Some threads included in the script refer to dynamic filters and reactive plots). No doubt things will have changed by now 

Just a couple of highlights about this simple script:

- The main thing the script accomplishes is to re-index nodes and arcs reactively, based on the user’s selection of which groups of nodes should be visualised. In doing so it is useful to keep in mind that the `networkD3 ` library requires that nodes are zero-indexed (something I was unaware of at the time).

- Another issue worth mentioning is that, if one relies on the `colourScale` argument of the `forceNetwork` function, the color-coding of the node groups may “shift” as the user selects which group of nodes to visualise. This is due to the fact that the D3 function `scaleOrdinal` maps the first element in the node group domain onto the first colour available in the palette and so on - see e.g. [this resource](https://d3js.org/d3-scale/ordinal#scaleOrdinal). I fixed this issue by doing some sub-setting of both node domain and colour palette before passing them as vectors to `scaleOrdinal`, with the aid of the function `toJSON` in the package `jsonlite`.

One final caveat: the graph is generated randomly in the background using `igraph` infrastructure. Since the focus is on the ability to subset the graph interactively, I didn’t make the parameters that generate the graph itself interactive. 
