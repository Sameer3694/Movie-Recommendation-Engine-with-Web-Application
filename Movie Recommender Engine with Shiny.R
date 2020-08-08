#Recommender System With Shiny 

library(shiny) #to make the web-application
library(dplyr) #to filter/ operate on dataframes
library(reshape2) #to make differnt user-item matrix
library(Matrix) #for creating sparse matrix 
library(recommenderlab) #for creating real rating matrix and run recommendation algorithm

#read the data
ratings <- read.csv("/Users/sameerrathi/Documents/MyProjects/Kaggle/Movie Dataset/3405_6663_bundle_archive/ratings_small.csv")
movies <- read.csv("/Users/sameerrathi/Documents/MyProjects/Kaggle/Movie Dataset/3405_6663_bundle_archive/movies_metadata.csv")

#looking at the structure of the two dataframes
str(movies)
str(ratings)

#creating matrix of users and movies with ratings as their values
rating_matrix <- ratings %>% dcast(userId ~ movieId,value.var = 'rating')

#replacing NAs with 0s
row.names(rating_matrix) <- rating_matrix[,'userId']
new_matrix <- rating_matrix[,-1]
new_matrix <- as.matrix(new_matrix %>% replace(is.na(.),0))

#taking out distinct title for display
unique.movies<- movies %>% distinct(id,title) 
unique.movies <- as.array(unique.movies$title)

#creating sparse Matrix
sparse.matrix <- as(new_matrix,'sparseMatrix')

#creating Final S4 Rating Matrix
rating.matrix <- new('realRatingMatrix',data = sparse.matrix)

#building a recommender system model
model1  <- Recommender(rating.matrix, method = 'UBCF', param = list(method = 'pearson',nn = 20))

#building item similarity matrix --- 
#Not displayed on the Web App as it is taking a lot of time to load on local machine

# item_matrix <- ratings %>% dcast(movieId~userId ,value.var = 'rating')
# row.names(item_matrix) <- item_matrix[,'movieId']
# new_i_matrix <- item_matrix[,-1]
# new_i_matrix <- as.matrix(new_i_matrix %>% replace(is.na(.),0))
# sparse.i.matrix <- as(new_i_matrix,'sparseMatrix')
# i.matrix <- new('realRatingMatrix',data = sparse.i.matrix)
# sim  <- i.matrix %>% normalize() %>% similarity(method = 'pearson') %>% as.matrix()

  #in the output render function (if needed on the web applications)
  # temp2 <- as.character(input$Movie)
  # temp3 <- movies %>% filter(title==temp2) %>% distinct(id)
  # movie.id <- as.integer(temp3[1,1])
  # sim<- sim[,movie.id] %>% as.data.frame()
  # sim$movie  <- rownames(sim)
  # colnames(sim)[1]  <- 'similarity'
  # sim <- sim %>% arrange(similarity %>% desc())
  # movies$id<-as.integer((as.character(movies$id)))
  # sim$movie <- as.integer(sim$movie)
  # Top10movies <- inner_join(sim,movies,by=c("movie"='id')) %>% select(title) %>% head(10)
  # names(Top10movies)[1] <- "Similar Movies"
  # Top10movies

#this model made with 20 nearest neighbors

#evaluating and comapring with other model(s):
evaluating  <- evaluationScheme(rating.matrix, method = 'cross-validation',k = 5, given = -1,goodRating = 5)

models <- list('UCF_10' = list(name = 'UBCF',param = list(nn = 10)),
               'UCF_15' = list(name = 'UBCF',param = list(nn = 15)),
               'UCF_20' = list(name = 'UBCF',param = list(nn = 20)))
set.seed(12345)

result.list <- evaluate(evaluating, method = models, type = 'ratings')
results <- avg(results.list) %>% unlist()
results <- as.data.frame(results)
labels <- c('RMSE','MSE','MAE')
results$Labels <- labels
results$method <- row.names(results)
names(results)[1] <- "Error Value"

#We will only consider RMSE in this comparison
results <- results %>% filter(Lables=='RMSE')

print(results) #the least RMSE comes with 20 nn and therefore we use it for our final model.

#We can also try "Random" and "Popularity" methods of the recommender lab.

#creating a web application using shiny

ui <- fluidPage(
  titlePanel("Movie Recommendation Engine"), # title output
  sidebarLayout(
    sidebarPanel(selectInput("Age","Are you 18 years old or above?", choices=c("Yes","No")),
                 sliderInput("ID", "Select User ID", min=1,max=671, value=1),
                 selectInput("Movie","Select the movie", choices=unique.movies),
  ),
  mainPanel(
    textOutput("profile"),
    tableOutput('table_top_10_movies')
 )

)

)

server <- function(input, output){
  
  output$profile <- renderText({
    paste("Here are some movie suggestions based on user profile")
  })
  
  output$table_top_10_movies <- renderTable({
    temp1 <- as.integer(input$ID)
    user <- predict(model1, rating.matrix[temp1,], type='ratings')
    ratingsforuser <- as(user,'data.frame') %>% arrange(desc(rating) ) %>%filter(rating>3) %>% mutate(rank=row.names(.))
    watchedbyuser <- ratings %>% filter(userId==temp1) %>% select(movieId)
    watchedbyuser <- as.array(watchedbyuser$movieId)
    temp <- ratingsforuser %>% mutate(watched= ifelse(ratingsforuser$item %in% watchedbyuser,1,0)) %>% filter(watched==0)
    top10movies <- as.data.frame(as.integer((as.character(temp$item))))
    names(top10movies)[1] <- "movieId"
    movies$id<-as.integer((as.character(movies$id)))
    BestResults <- inner_join(top10movies,movies,by=c("movieId"='id')) %>% select(title) %>% head(10)
    names(BestResults)[1]<-"Movies you may like"
    BestResults
  })
  
}

shinyApp(ui = ui, server = server)

# To provide a better recommendation, ensembelled methods can be used. 
# We can use both collabrorative and content based filtering in such cases.




