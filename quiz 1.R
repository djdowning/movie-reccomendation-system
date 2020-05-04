dim(edx)


#Q2
#How many zeros were given as ratings in the edx dataset?
edx %>% group_by(rating) %>% summarize(n=n())
sum(edx$rating=="0")

   
#How many threes were given as ratings in the edx dataset?
edx %>% filter(rating == 3) %>% tally()

head(edx)

#Q3
#How many different movies are in the edx dataset?
edx %>% group_by(movieId) %>% summarize(n=n())
n_distinct(edx$movieId)


#Q4
#How many different users are in the edx dataset?
n_distinct(edx$userId)

#Q5
#How many movie ratings are in each of the following genres in the edx dataset?
#Drama:
#Comedy:
#Thriller:
#Romance:
edx %>% filter(str_detect(genres,"Drama")) %>% tally()
edx %>% filter(str_detect(genres,"Comedy")) %>% tally()
edx %>% filter(str_detect(genres,"Thriller")) %>% tally()
edx %>% filter(str_detect(genres,"Romance")) %>% tally()

# str_detect
genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})


#Q6
#Which movie has the greatest number of ratings?

edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))



#Q7
#What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>%
  summarize(count = n()) %>%
  top_n(5) %>%
  arrange(desc(count))

#Q8
#True or False: In general, half star ratings are less common than whole star ratings (e.g., there are fewer ratings of 
#  3.5 than there are ratings of 3 or 4, etc.).

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()


