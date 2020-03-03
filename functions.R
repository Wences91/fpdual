require(dplyr)
require(igraph)
require(zoo)

co_mentions <- function(mentions){
    mentions <- mentions[,c('author', 'mentioned', 'tweet_id')]
    mentions <- dplyr::inner_join(x=mentions, y=mentions,  by='author')
    mentions <- mentions[which(!(mentions$mentioned.x==mentions$mentioned.y)),]

    mentions <- dplyr::mutate(mentions,
                              Source=pmin(mentioned.x, mentioned.y),
                              Target=pmax(mentioned.x, mentioned.y))

    mentions <- mentions[, which(names(mentions) %in% c('Source', 'Target', 'author'))]
    mentions <- dplyr::distinct(mentions, .keep_all=TRUE)
    mentions <- mentions[, which(names(mentions) %in% c('Source', 'Target'))]

    mentions$Weight <- 1
    mentions <- dplyr::group_by(mentions, Source, Target)
    mentions <- dplyr::summarise(mentions, Weight=sum(Weight))
    mentions <- data.frame(mentions)

    return(mentions)
}

# This function return the main component of a network
giant.component <- function(graph){
  # divide into clusters
  cl <- clusters(graph)
  # return the the largest cluster
  return(induced.subgraph(graph, which(cl$membership == which.max(cl$csize))))
}

# This function calculates some local network indicators and return top nodes basis on them
top_network_nodes <- function(edges, percen=0.05, main_component=FALSE){
  # create the data.frame which will summarise all information
  data_summary <- data.frame(Indicators=character(), Top=character(),
                             Cumulative=character(), Accounts=character(),
                             `Unique accounts`=character(),
                             check.names = FALSE, stringsAsFactors = FALSE)
  data_summary[1:3,] <- NA
  data_summary$Indicators <- c('Degree', 'Betweeness', 'Eigenvector')
  data_summary$Top <- 100*percen
  
  
  # create the network
  g <- igraph::graph_from_data_frame(edges, directed = TRUE)
  
  if(main_component){
    g <- giant.component(g)
  }
  
  # determinate the number of top nodes
  top_actors <- trunc(length(V(g))*percen)
  cat(top_actors, 'top accounts\n')
  
  # calculate the local network indicators
  V(g)$degree <- igraph::degree(g)
  V(g)$betweeness <- igraph::betweenness(g)
  V(g)$eigen <- igraph::eigen_centrality(g, directed = TRUE)$vector
  
  # select top nodes
  top_d <- V(g)$name[order(V(g)$degree, decreasing = TRUE)][1:top_actors]
  top_b <- V(g)$name[order(V(g)$betweeness, decreasing = TRUE)][1:top_actors]
  top_e <- V(g)$name[order(V(g)$eigen, decreasing = TRUE)][1:top_actors]
  
  # fill summary data.frame
  data_summary$`Unique accounts`[which(data_summary$Indicators == 'Degree')] <- sum(!(top_d %in% c(top_b, top_e)))
  data_summary$`Unique accounts`[which(data_summary$Indicators == 'Betweeness')] <- sum(!(top_b %in% c(top_d, top_e)))
  data_summary$`Unique accounts`[which(data_summary$Indicators == 'Eigenvector')] <- sum(!(top_e %in% c(top_b, top_d)))
  
  
  # calcute total top nodes 
  top_aux <- unique(c(top_d, top_b))
  tops <- unique(c(top_aux, top_e))
  data_summary$Accounts[which(data_summary$Indicators == 'Degree')] <- top_actors
  data_summary$Accounts[which(data_summary$Indicators == 'Betweeness')] <- length(top_aux)
  data_summary$Accounts[which(data_summary$Indicators == 'Eigenvector')] <- length(tops)
  
  # calcute the cumulative frequency of top nodes by indicator 
  data_summary$Cumulative[which(data_summary$Indicators == 'Degree')] <- round(100*sum(V(g)$degree[which(V(g)$name %in% tops)])/sum(V(g)$degree), 2)
  data_summary$Cumulative[which(data_summary$Indicators == 'Betweeness')] <- round(100*sum(V(g)$betweeness[which(V(g)$name %in% tops)])/sum(V(g)$betweeness), 2)
  data_summary$Cumulative[which(data_summary$Indicators == 'Eigenvector')] <- round(100*sum(V(g)$eigen[which(V(g)$name %in% tops)])/sum(V(g)$eigen), 2)
  
  return(list(summary=data_summary, top_accounts=tops, network=g))
}

# This function creates missing dates and calculate frequencies
full_freq_dates <- function(tweets){
    # calculate tweets frequencies
    freq_tweets <- as.data.frame(table(as.Date(tweets, format='%d-%m-%Y')), stringsAsFactors = FALSE)
    freq_tweets$Var1 <- as.Date(freq_tweets$Var1)
    
    # create the missing dates
    empty_tweets <- data.frame(Var1 = as.Date(format(seq.POSIXt(as.POSIXct(min(freq_tweets$Var1)), as.POSIXct(max(freq_tweets$Var1)), by='day'), '%d-%m-%Y'), '%d-%m-%Y'),
                           stringsAsFactors = FALSE)
    
    # join two previus data.frame
    freq_tweets <- dplyr::full_join(empty_tweets, freq_tweets, by='Var1')
    
    # transform missing dates NA frequencies into 0
    freq_tweets$Freq[which(is.na(freq_tweets$Freq))] <- 0
    
    return(freq_tweets)
}

mon_ann_freq <- function(freq_tweets, by='month'){
    # this function considers that this is the order
    names(freq_tweets) <- c('Dates', 'Freq')
    
    if(tolower(by) == 'month'){
        freq_tweets$Dates <- as.Date(zoo::as.yearmon(freq_tweets$Dates), frac=1)
    }else if(tolower(by) == 'annual'){
        freq_tweets$Dates <- format(as.Date(zoo::as.yearmon(freq_tweets$Dates)), '%Y', frac=1)
    }else{
        cat('Error', by)
        return(NA)
    }
    
    freq_tweets <- dplyr::group_by(freq_tweets, Dates)
    freq_tweets <- dplyr::summarise(freq_tweets, Freq = sum(Freq))
    return(freq_tweets)
}

# this function calculates basic statistics
basic_stats <- function(frequencies){
    cat('Average', round(mean(frequencies), 2), '\n')
    cat('SD', round(sd(frequencies), 2), '\n')
    cat('Median', round(median(frequencies), 2), '\n')
    cat('Total', sum(frequencies), '\n')
}


# network top accounts stats
top_accounts_net_stats <- function(network, top_accounts){
    sapply(sort(unique(top_accounts$field)), function(x){
        cat(x, '\n')
        cat('Degree',round(100*sum(V(network)$degree[which(V(network)$name %in% top_accounts$account[which(top_accounts$field == x)])])/sum(V(network)$degree), 2), ' ')
        cat('Betweeness',round(100*sum(V(network)$betweeness[which(V(network)$name %in% top_accounts$account[which(top_accounts$field == x)])])/sum(V(network)$betweeness), 2), ' ')
        cat('Eigen',round(100*sum(V(network)$eigen[which(V(network)$name %in% top_accounts$account[which(top_accounts$field == x)])])/sum(V(network)$eigen), 2), '\n')  
})
    cat('Total', '\n')
    cat('Degree',round(100*sum(V(network)$degree[which(V(network)$name %in% top_accounts$account)])/sum(V(network)$degree), 2), ' ')
    cat('Betweeness',round(100*sum(V(network)$betweeness[which(V(network)$name %in% top_accounts$account)])/sum(V(network)$betweeness), 2), ' ')
    cat('Eigen',round(100*sum(V(network)$eigen[which(V(network)$name %in% top_accounts$account)])/sum(V(network)$eigen), 2), '\n')
}

# accounts by tweets
top_tweet <- function(tweets){
    # this function return a data.frame with all accounts and the number of tweets
    tops <- as.data.frame(table(tweets), stringsAsFactors = FALSE)
    names(tops) <- c('Account', 'Tweets')
    tops <- tops[order(tops$Tweets, decreasing = TRUE),]
    row.names(tops) <- as.character(1:dim(tops)[1])
    return(tops)
}

# accounts by mentions
top_mentioned <- function(tweets){
    # this function return a data.frame with all accounts and the number of mentions received
    tops <- as.data.frame(table(tweets), stringsAsFactors = FALSE)
    names(tops) <- c('Account', 'Mentions')
    tops <- tops[order(tops$Mentions, decreasing = TRUE),]
    row.names(tops) <- as.character(1:dim(tops)[1])
    return(tops)
}

# top accounts tweets by year
top_accounts_stats <- function(tweets, top_accounts){
    # this function calculates the number of annual tweets by account and type
    tweets_year <- tweets
    tweets_year$date <- format(as.Date(tweets_year$date, format='%d-%m-%Y'), '%Y')
    tweets_year <- dplyr::inner_join(tweets_year, top_accounts, by=c('author'='account'))
    # create the result data.frame
    result <- as.data.frame(matrix(NA, nrow=length(unique(tweets_year$field)), ncol=length(unique(tweets_year$date)), dimnames = list(c(sort(unique(tweets_year$field))), c(sort(unique(tweets_year$date))))))
    sapply(unique(tweets_year$date), function(x){
        sapply(sort(unique(tweets_year$field)), function(y){
            result[y,x] <<- sum(tweets_year$date[which(tweets_year$field == y)] == x)
        })
    })
    result['Total',] <- colSums(result)
    return(result)
}

# top accounts tweets average by year
top_accounts_net_average <- function(tweets, top_accounts){
    # this function calculates the average of annual tweets by account and type
    tweets_year <- tweets
    tweets_year$date <- format(as.Date(tweets_year$date, format='%d-%m-%Y'), '%Y')
    tweets_year <- dplyr::inner_join(tweets_year, top_accounts, by=c('author'='account'))
    # create the result data.frame
    result <- as.data.frame(matrix(NA, nrow=length(unique(tweets_year$field)), ncol=length(unique(tweets_year$date)), dimnames = list(c(sort(unique(tweets_year$field))), c(sort(unique(tweets_year$date))))))
    sapply(unique(tweets_year$date), function(x){
        sapply(sort(unique(tweets_year$field)), function(y){
            result[y,x] <<- round(sum(tweets_year$date[which(tweets_year$field == y)] == x)/length(unique(tweets_year$author[which(tweets_year$field == y & tweets_year$date == x)])),2)
        })
    })
    sapply(unique(tweets_year$date), function(x){
            result['Total',x] <<- round(sum(tweets_year$date == x)/length(unique(tweets_year$author[which(tweets_year$date == x)])),2)
        })
    return(result)
}

# new top accounts tweets by year
top_accounts_net_accounts <- function(tweets, top_accounts){
    # this function calculates the number of new accounts by year and type
    tweets_year <- tweets
    tweets_year$date <- format(as.Date(tweets_year$date, format='%d-%m-%Y'), '%Y')
    tweets_year <- dplyr::inner_join(tweets_year, top_accounts, by=c('author'='account'))
    # create the result data.frame
    result <- as.data.frame(matrix(NA, nrow=length(unique(tweets_year$field)), ncol=length(unique(tweets_year$date)), dimnames = list(c(sort(unique(tweets_year$field))), c(sort(unique(tweets_year$date))))))
    sapply(unique(tweets_year$date), function(x){
        sapply(sort(unique(tweets_year$field)), function(y){
            result[y,x] <<- length(unique(tweets_year$author[which(tweets_year$field == y & tweets_year$date == x)]))
        })
    })
    sapply(unique(tweets_year$date), function(x){
            result['Total',x] <<- length(unique(tweets_year$author[which(tweets_year$date == x)]))
    })
    return(result)
}

# new unique top accounts tweets by year
top_accounts_net_uni_accounts <- function(tweets, top_accounts){
    # this function calculates the number of new unique accounts by year and type
    tweets_year <- tweets
    tweets_year$date <- format(as.Date(tweets_year$date, format='%d-%m-%Y'), '%Y')
    tweets_year <- dplyr::inner_join(tweets_year, top_accounts, by=c('author'='account'))
    # create the result data.frame
    result <- as.data.frame(matrix(NA, nrow=length(unique(tweets_year$field)), ncol=length(unique(tweets_year$date)), dimnames = list(c(sort(unique(tweets_year$field))), c(sort(unique(tweets_year$date))))))
    sapply(unique(tweets_year$date), function(x){
        sapply(sort(unique(tweets_year$field)), function(y){
            result[y,x] <<- sum(!(unique(tweets_year$author[which(tweets_year$field == y & tweets_year$date == x)]) %in% unique(tweets_year$author[which(tweets_year$field == y & tweets_year$date < x)])))
        })
    })
    sapply(unique(tweets_year$date), function(x){
            result['Total',x] <<- sum(!(unique(tweets_year$author[which(tweets_year$date == x)]) %in% unique(tweets_year$author[which(tweets_year$date < x)])))
    })
    return(result)
}


period_stats <- function(tweets, mentions){
    cat('Active actors:',length(unique(tweets$author)),'\n')
    cat('Active mentions:',length(unique(mentions$author)),'\n')
    cat('Mentions actors:',length(unique(mentions$mentioned)),'\n')
    cat('Mentions:',dim(mentions)[1],'\n')
    cat('Total actors:',length(unique(c(tweets$author, mentions$mentioned))),'\n')
}

period_stats_cumulative <- function(tweets, mentions){
    cat('Active actors:',length(unique(tweets$author)),'\n')
    cat('Active mentions:',length(unique(mentions$author)),'\n')
    cat('Mentions actors:',length(unique(mentions$mentioned)),'\n')
    cat('Mentions:',dim(mentions)[1],'\n')
    cat('Total actors:',length(unique(c(tweets$author, mentions$target))),'\n')
}

