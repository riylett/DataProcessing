#Reading the data
data_names <- c('Badges', 'Comments', 'PostLinks', 'Posts', 'Tags', 'Users', 'Votes')
for( d in data_names ){
  assign(d, read.csv(paste(d, '.csv', sep='')))
}



################Task 1 base R################
DupPostLinks <- PostLinks[PostLinks$LinkTypeId ==3,] #Duplicated posts have LinkTypeId 3
DupPosts <- merge(DupPostLinks, Posts, by.x = 'PostId', by.y = 'Id')
#After the merge the same PostId is on the table multiple times because of the differing
#RelatedPostId values which is kinda weird that a post is a duplicate of 3 different posts
#but I am assuming that the PostId is the post marked as a duplicate.
DupPosts <- DupPosts[!duplicated(DupPosts$PostId), ] 
#Removing the occurrences of the same PostId.
DupCount <- as.data.frame(table(DupPosts$OwnerUserId))
colnames(DupCount) <- c("UserId", "DuplicatePostsCount")
DupCount <- DupCount[order(DupCount$DuplicatePostsCount,decreasing = TRUE), ]
Top10DupUsers <- head(DupCount, 10)

Top10DupUsers <- merge(Top10DupUsers, Users[c('Id', 'DisplayName')], 
                       by.x = 'UserId', by.y = 'Id', all.x = TRUE)
QuestionCount <- as.data.frame(table(Posts$OwnerUserId[Posts$PostTypeId == 1]))
colnames(QuestionCount) <- c("UserId", "QuestionCount")
AnswerCount <- as.data.frame(table(Posts$OwnerUserId[Posts$PostTypeId == 2]))
colnames(AnswerCount) <- c("UserId", "AnswerCount")
CommentCount <- as.data.frame(table(Comments$UserId))
colnames(CommentCount) <- c("UserId", "CommentCount")
Score <- aggregate(Posts$Score, Posts['OwnerUserId'], sum)
#I am assuming that the overall score mentioned in the task is the sum of the scores
#of the posts of the user.
colnames(Score) <- c("UserId", "Score")

Top10Counts <- Reduce(function(x, y) merge(x, y, by = "UserId", all.x = TRUE),
                       list(Top10DupUsers, QuestionCount, AnswerCount, CommentCount, 
                            Score))
Top10Counts <- Top10Counts[order(Top10Counts$DuplicatePostsCount,decreasing = TRUE), ]


################Task 1 dplyr################
library(dplyr)
DupPosts2 <- PostLinks %>%
  filter(LinkTypeId == 3) %>%
  left_join(Posts, by = c('PostId' = 'Id')) %>%
  distinct(PostId, .keep_all = TRUE) 
#Just like the base R  implementation removing the occurrences of the same PostId.

Top10DupUsers2 <- DupPosts2 %>%
  count(OwnerUserId) %>%
  rename(DuplicatePostsCount = n) %>%
  arrange(desc(DuplicatePostsCount)) %>%
  slice(1:10) %>%
  left_join(Users %>% select(Id, DisplayName), by = c("OwnerUserId" = "Id"))

QuestionCount2 <- Posts %>%
  filter(PostTypeId == 1) %>%
  count(OwnerUserId) %>%
  rename(QuestionCount = n)

AnswerCount2 <- Posts %>%
  filter(PostTypeId == 2) %>%
  count(OwnerUserId) %>%
  rename(AnswerCount = n)

CommentCount2 <- Comments %>%
  count(UserId) %>%
  rename(OwnerUserId = UserId, CommentCount = n)

Score2 <- Posts %>%
  group_by(OwnerUserId) %>%
  summarise(Score = sum(Score, na.rm = TRUE))

Top10Counts2 <- Top10DupUsers2 %>%
  left_join(QuestionCount2, by = "OwnerUserId") %>%
  left_join(AnswerCount2, by = "OwnerUserId") %>%
  left_join(CommentCount2, by = "OwnerUserId") %>%
  left_join(Score2, by = "OwnerUserId") %>%
  arrange(desc(DuplicatePostsCount))


################Task 1 equivalence comparison################
compare::compare(Top10Counts, Top10Counts2,allowAll = TRUE)
#Output:
#TRUE
# [UserId] coerced from <integer> to <factor>
# sorted
# renamed
# [USERID] dropped [unused] levels
# renamed rows
# dropped names
# dropped row names


################Task 1 execution time comparison################
library(microbenchmark)
microbenchmark(
  base = {
    DupPostLinks <- PostLinks[PostLinks$LinkTypeId ==3,]
    DupPosts <- merge(DupPostLinks, Posts, by.x = 'PostId', by.y = 'Id')
    DupPosts <- DupPosts[!duplicated(DupPosts$PostId), ]
    DupCount <- as.data.frame(table(DupPosts$OwnerUserId))
    colnames(DupCount) <- c("UserId", "DuplicatePostsCount")
    DupCount <- DupCount[order(DupCount$DuplicatePostsCount,decreasing = TRUE), ]
    Top10DupUsers <- head(DupCount, 10)
    Top10DupUsers <- merge(Top10DupUsers, Users[c('Id', 'DisplayName')], 
                           by.x = 'UserId', by.y = 'Id', all.x = TRUE)
    QuestionCount <- as.data.frame(table(Posts$OwnerUserId[Posts$PostTypeId == 1]))
    colnames(QuestionCount) <- c("UserId", "QuestionCount")
    AnswerCount <- as.data.frame(table(Posts$OwnerUserId[Posts$PostTypeId == 2]))
    colnames(AnswerCount) <- c("UserId", "AnswerCount")
    CommentCount <- as.data.frame(table(Comments$UserId))
    colnames(CommentCount) <- c("UserId", "CommentCount")
    Score <- aggregate(Posts$Score, Posts['OwnerUserId'], sum)
    colnames(Score) <- c("UserId", "Score")
    Top10Counts <- Reduce(function(x, y) merge(x, y, by = "UserId", all.x = TRUE),
                          list(Top10DupUsers, QuestionCount, AnswerCount, CommentCount, 
                               Score))
    Top10Counts <- Top10Counts[order(Top10Counts$DuplicatePostsCount,decreasing = TRUE), ]
  },
  dplyr = {
    DupPosts2 <- PostLinks %>%
      filter(LinkTypeId == 3) %>%
      left_join(Posts, by = c('PostId' = 'Id')) %>%
      distinct(PostId, .keep_all = TRUE)
    Top10DupUsers2 <- DupPosts2 %>%
      count(OwnerUserId) %>%
      rename(DuplicatePostsCount = n) %>%
      arrange(desc(DuplicatePostsCount)) %>%
      slice(1:10) %>%
      left_join(Users %>% select(Id, DisplayName), by = c("OwnerUserId" = "Id"))
    QuestionCount2 <- Posts %>%
      filter(PostTypeId == 1) %>%
      count(OwnerUserId) %>%
      rename(QuestionCount = n)
    AnswerCount2 <- Posts %>%
      filter(PostTypeId == 2) %>%
      count(OwnerUserId) %>%
      rename(AnswerCount = n)
    CommentCount2 <- Comments %>%
      count(UserId) %>%
      rename(OwnerUserId = UserId, CommentCount = n)
    Score2 <- Posts %>%
      group_by(OwnerUserId) %>%
      summarise(Score = sum(Score, na.rm = TRUE))
    Top10Counts2 <- Top10DupUsers2 %>%
      left_join(QuestionCount2, by = "OwnerUserId") %>%
      left_join(AnswerCount2, by = "OwnerUserId") %>%
      left_join(CommentCount2, by = "OwnerUserId") %>%
      left_join(Score2, by = "OwnerUserId") %>%
      arrange(desc(DuplicatePostsCount))
  }, times = 5
) -> r1
   
summary(r1)
#Output:
#   expr      min       lq     mean   median       uq      max neval cld
#1  base 171.3369 183.3305 197.7948 183.3465 189.3017 261.6583     5   a
#2 dplyr 185.1510 194.3214 206.8530 196.8311 222.1837 235.7779     5   a




################Task 2 base R################
Posts8PM6AM <- Posts[(as.integer(substr(Posts$CreationDate, 12, 13)) >= 20) | 
                       (as.integer(substr(Posts$CreationDate, 12, 13)) < 6), ]

UserPosts <- merge(Users[c('Id', 'Location')], Posts8PM6AM, by.x = 'Id', by.y = 'OwnerUserId')

UserPostCounts <- aggregate(UserPosts$Id,list(UserPosts$Id, UserPosts$Location), length)
colnames(UserPostCounts) <- c("UserId", "Location", "PostCount")

UserPostCounts <- UserPostCounts[order(UserPostCounts$PostCount, decreasing = TRUE),]


################Task 2 dplyr################
library(dplyr)
Posts8PM6AM2 <- Posts %>%
  filter((as.integer(substr(CreationDate, 12, 13)) >= 20) | 
           (as.integer(substr(CreationDate, 12, 13)) < 6))

UserPosts2 <- Posts8PM6AM2 %>%
  left_join(Users %>% select(Id, Location), by = c("OwnerUserId" = "Id"))

UserPostCounts2 <- UserPosts2 %>%
  group_by(OwnerUserId, Location) %>%
  filter(!is.na(OwnerUserId)) %>%
  summarise(PostCount = n(),.groups = 'drop') %>%
  arrange(desc(PostCount))
  


################Task 2 equivalence comparison################
compare::compare(UserPostCounts, as.data.frame(UserPostCounts2),allowAll = TRUE)
#Output:
#TRUE
# sorted
# renamed
# renamed rows
# dropped names
# dropped row names


################Task 2 execution time comparison################
library(microbenchmark)
microbenchmark(
  base = {
    Posts8PM6AM <- Posts[(as.integer(substr(Posts$CreationDate, 12, 13)) >= 20) | 
                           (as.integer(substr(Posts$CreationDate, 12, 13)) < 6), ]
    
    UserPosts <- merge(Users[c('Id', 'Location')], Posts8PM6AM, by.x = 'Id', by.y = 'OwnerUserId')
    
    UserPostCounts <- aggregate(UserPosts$Id,list(UserPosts$Id, UserPosts$Location), length)
    colnames(UserPostCounts) <- c("UserId", "Location", "PostCount")
    
    UserPostCounts <- UserPostCounts[order(UserPostCounts$PostCount, decreasing = TRUE),]
  },
  dplyr = {
    Posts8PM6AM2 <- Posts %>%
      filter((as.integer(substr(CreationDate, 12, 13)) >= 20) | 
               (as.integer(substr(CreationDate, 12, 13)) < 6))
    
    UserPosts2 <- Posts8PM6AM2 %>%
      left_join(Users %>% select(Id, Location), by = c("OwnerUserId" = "Id"))
    
    UserPostCounts2 <- UserPosts2 %>%
      group_by(OwnerUserId, Location) %>%
      filter(!is.na(OwnerUserId)) %>%
      summarise(PostCount = n(),.groups = 'drop') %>%
      arrange(desc(PostCount))
  }, times = 5
) -> r2

summary(r2)

#Output:
#   expr      min       lq     mean   median       uq      max neval cld
#1  base  96.2378  97.1533 107.6692  98.3394 104.9996 141.6160     5   a
#2 dplyr 106.5376 110.2012 119.4991 111.2632 112.5193 156.9742     5   a




################Task 3################
#The result of the sql query is the top 10 users with the highest average answer
#count for the questions they posted. The table has columns AccountId, DisplayName,
#Location, and AverageAnswersCount.

################Task 3 dplyr################
library(dplyr)

AnsCount <- Posts %>%
  filter(PostTypeId == 2) %>%  
  select(ParentId) %>%  
  group_by(ParentId) %>%                         
  summarise(AnswersCount = n())  

PostAuth <- AnsCount %>%
  inner_join(Posts, by = c("ParentId" = "Id")) %>%   
  select(AnswersCount, ParentId, OwnerUserId)            

r3 <- PostAuth %>%
  inner_join(Users, by = c("OwnerUserId" = "AccountId"), relationship = "many-to-many") %>%
  group_by(OwnerUserId,DisplayName,Location) %>%              
  summarise(AverageAnswersCount = mean(AnswersCount, na.rm = TRUE),.groups = 'drop') %>% 
  arrange(desc(AverageAnswersCount), desc(OwnerUserId)) %>%                       
  slice(1:10)

#I was getting a warning message about many-to-many relationship with the inner_join 
#with PostAuth and Users only for 2 rows. I think it's weird that there is a many to many
#relationship but it does not affect the result.

r3 <- as.data.frame(r3)

################Task 3 SQL################
library(sqldf)

sql_result <- sqldf("SELECT
                     Users.AccountId,
                     Users.DisplayName,
                     Users.Location,
                     AVG(PostAuth.AnswersCount) as AverageAnswersCount
                     FROM
                     (
                       SELECT
                       AnsCount.AnswersCount,
                       Posts.Id,
                       Posts.OwnerUserId
                       FROM (
                         SELECT Posts.ParentId, COUNT(*) AS AnswersCount
                         FROM Posts
                         WHERE Posts.PostTypeId = 2
                         GROUP BY Posts.ParentId
                       ) AS AnsCount
                       JOIN Posts ON Posts.Id = AnsCount.ParentId
                     ) AS PostAuth
                     JOIN Users ON Users.AccountId=PostAuth.OwnerUserId
                     GROUP BY OwnerUserId
                     ORDER BY AverageAnswersCount DESC
                     LIMIT 10")

################Task 3 equivalence comparison################
compare::compare(r3, sql_result,allowAll = TRUE)
#Output:
#TRUE
# renamed
# dropped names
