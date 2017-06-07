


makeAccessToken <- function(token_file_name) {
  ok <- tryCatch(token_file_name, error = function(e) {cat('No token_file_name argument provided!\n'); return(FALSE)})
  if (ok != FALSE) {
    background_check <- readline(prompt="Have you read instructions on the help page for this function? Type y if yes: ")
    if (background_check != 'y' & background_check != 'yes' ) {
      cat('Please, type ?makeAccessToken and read instructions in the Details section first. Then call this function again.\n')
    } else{
      appid <- readline(prompt="Paste here the application id you got from VK: ")
      cat("\nYou're going to be redirected to a webpage. Copy its URL and paste it below.\n")
      Sys.sleep(1)
      browseURL(paste0('https://oauth.vk.com/authorize?client_id=', appid,'&display=page&redirect_uri=https://oauth.vk.com/blank.html&scope=offline&response_type=token&v=5.52'))
      Sys.sleep(1)
      new_url <- readline(prompt="Paste it here: ")
      st <- regexpr(pattern = 'access_token=', text = new_url)
      st <- as.numeric(st) + attributes(st)$match.length
      fin <- as.numeric(regexpr(pattern = '&expires_in', text = new_url))-1
      token <- substr(x = new_url, start = st, stop = fin)
      write(x = token, file = token_file_name)
    }
  }
}


#----------------- USER -----------------#
getUserInfo <- function(user_ids, access_token, num_universities = 1, num_schools = 1, num_jobs = 1) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/users.get?user_ids=', user_ids,'&fields=photo_id,verified,sex,bdate,city,country,home_town,has_photo,photo_100,has_mobile,contacts,site,education,universities,schools,status,last_seen,followers_count,common_count,occupation,relatives,relation,personal,connections,wall_comments,activities,interests,music,movies,tv,books,games,about,quotes,timezone,screen_name,maiden_name,is_friend,friend_status,career,military&v=5.64&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    items <- fetched$response
    
    output <- data.frame('id' = items$id, 
                         'first_name' = items$first_name,
                         'last_name' = items$last_name, stringsAsFactors = F)
    if ('sex' %in% names(items)) {
      output$sex <- items$sex
      output$sex[output$sex == 0] <- NA
      output$sex[output$sex == 1] <- 'female'
      output$sex[output$sex == 2] <- 'male'
    }
    if ('screen_name' %in% names(items)) {
      output$screen_name <- items$screen_name
    }
    if ('bdate' %in% names(items)) {
      output$bdate <- items$bdate
    }
    if ('city' %in% names(items)) {
      output$city <- items$city$title
    }
    if ('country' %in% names(items)) {
      output$country <- items$country$title
    }
    if ('relation' %in% names(items)) {
      output$relation <- items$relation
    }
    if ('relation_partner' %in% names(items)) {
      output$relation_partner_id <- items$relation_partner$id
    }
    if ('has_photo' %in% names(items)) {
      output$has_photo <- items$has_photo
    }
    if ('wall_comments' %in% names(items)) {
      output$num_wall_comments <- items$wall_comments
    }
    if ('photo_100' %in% names(items)) {
      output$photo_url <- items$photo_100
    }
    if ('has_mobile' %in% names(items)) {
      output$has_mobile <- items$has_mobile
    }
    if ('can_see_all_posts' %in% names(items)) {
      output$can_see_all_posts <- items$can_see_all_posts
    }
    if ('status' %in% names(items)) {
      output$status <- items$status
    }
    if ('last_seen' %in% names(items)) {
      output$last_seen <- items$last_seen$time
    }
    if ('universities' %in% names(items)) {
      output$number_universities <- nrow(items$universities[[1]])
      univ <- items$universities[[1]]
      if (length(nrow(univ)) > 0) {
        # check if graduation is available
        if ('graduation' %in% names(univ)) {
          univ <- univ[order(univ$graduation, decreasing = T),]
          output$university_last_graduation <- univ$graduation[1]
        } else {
          output$university_last_graduation <- NA
        }
        if ('name' %in% names(univ)) {
          output$university_last <- univ$name[1]
        } else {
          output$university_last <- NA
        }
        if ('faculty_name' %in% names(univ)) {
          output$university_last_department <- univ$faculty_name[1]
        } else {
          output$university_last_department <- NA
        }
        if ('education_status' %in% names(univ)) {
          output$university_last_degree <- univ$education_status[1]
        } else {
          output$university_last_degree <- NA
        }
        if (num_universities > 1) {
          for (i in 2:num_universities) {
            output[[paste0('university_', i, 'tolast_graduation')]] <- univ$graduation[i]
            output[[paste0('university_', i, 'tolast_department')]] <- univ$faculty_name[i]
            output[[paste0('university_', i, 'tolast_degree')]] <- univ$education_status[i]
          }
        }
      } else { # if univ is empty
        output$university_last <- NA
        output$university_last_graduation <- NA
        output$university_last_department <- NA
        output$university_last_degree <- NA
        if (num_universities > 1) {
          for (i in 2:num_universities) {
            output[[paste0('university_', i, 'tolast_graduation')]] <- NA
            output[[paste0('university_', i, 'tolast_department')]] <- NA
            output[[paste0('university_', i, 'tolast_degree')]] <- NA
          }
        }
      }
    }
    if ('schools' %in% names(items)) {
      output$number_schools <- nrow(items$schools[[1]])
      school <- items$schools[[1]]
      if (length(nrow(school)) > 0) {
        # check of year_to is in school
        if ('year_to' %in% names(school)) {
          school <- school[order(school$year_to, decreasing = T),]
          output$school_last_to_year <- school$year_to[1]
        } else {
          output$school_last_to_year <- NA
        }
        if ('name' %in% names(school)) {
          output$school_last <- school$name[1]
        } else {
          output$school_last <- NA
        }
        if ('year_from' %in% names(school)) {
          output$school_last_from_year <- school$year_from[1]
        } else {
          output$school_last_from_year <- NA
        }
        if ('country' %in% names(school)) {
          output$school_last_country <- school$country[1]
        } else {
          output$school_last_country <- NA
        }
        if ('city' %in% names(school)) {
          output$school_last_city <- school$city[1]
        } else {
          output$school_last_city <- NA
        }
        if (num_schools > 1) {
          for (i in 2:num_schools) {
            output[[paste0('school_', i, 'tolast_from_year')]] <- school$year_from[i]
            output[[paste0('school_', i, 'tolast_to_year')]] <- school$year_to[i]
            output[[paste0('school_', i, 'tolast_country')]] <- school$country[i]
            output[[paste0('school_', i, 'tolast_city')]] <- school$city[i]
          }
        }
      } else { # if school is empty
        output$school_last <- NA
        output$school_last_from_year <- NA
        output$school_last_to_year <- NA
        output$school_last_country <- NA
        output$school_last_city <- NA
        if (num_schools > 1) {
          for (i in 2:num_schools) {
            output[[paste0('school_', i, 'tolast_from_year')]] <- NA
            output[[paste0('school_', i, 'tolast_to_year')]] <- NA
            output[[paste0('school_', i, 'tolast_country')]] <- NA
            output[[paste0('school_', i, 'tolast_city')]] <- NA
          }
        }
      }
    }
    
    if ('career' %in% names(items) ) {
      output$number_jobs <- nrow(items$career[[1]])
      jobs <- items$career[[1]]
      if (length(nrow(jobs)) > 0) {
        if ('from' %in% names(jobs)) {
          jobs <- jobs[order(jobs$from, decreasing = T),]
          output$job_last_start_year <- jobs$from[1]
        } else {
          output$job_last_start_year <- NA
        }
        if ('until' %in% names(jobs)) {
          output$job_last_end_year <- jobs$until[1]
        } else {
          output$job_last_end_year <- NA
        }
        if ('company' %in% names(jobs)) {
          output$job_last <- jobs$company[1]
        } else {
          output$job_last <- NA
        }
        if ('position' %in% names(jobs)) {
          output$job_last_position <- jobs$position[1]
        } else {
          output$job_last_position <- NA
        }
        if ('city_id' %in% names(jobs)) {
          output$job_last_city <- jobs$city_id[1]
        } else {
          output$job_last_city <- NA
        }
        if ('country_id' %in% names(jobs)) {
          output$job_last_country <- jobs$country_id[1]
        } else {
          output$job_last_country <- NA
        }
        if (num_jobs > 1) {
          for (i in 2:num_jobs) {
            output[[paste0('job_', i, 'tolast_start_year')]] <- jobs$from[i]
            output[[paste0('job_', i, 'tolast_end_year')]] <- jobs$until[i]
            output[[paste0('job_', i, 'tolast_position')]] <- jobs$position[i]
            output[[paste0('job_', i, 'tolast_city')]] <- jobs$city_id[i]
            output[[paste0('job_', i, 'tolast_country')]] <- jobs$country_id[i]
          }
        }
      } else { # if jobs is empty
        output$job_last <- NA
        output$job_last_start_year <- NA
        output$job_last_end_year <- NA
        output$job_last_position <- NA
        output$job_last_city <- NA
        output$job_last_country <- NA
        if (num_jobs > 1) {
          for (i in 2:num_jobs) {
            output[[paste0('job_', i, 'tolast_start_year')]] <- NA
            output[[paste0('job_', i, 'tolast_end_year')]] <- NA
            output[[paste0('job_', i, 'tolast_position')]] <- NA
            output[[paste0('job_', i, 'tolast_city')]] <- NA
            output[[paste0('job_', i, 'tolast_country')]] <- NA
          }
        }
      }
    }
    return(output)
  }
}


getMultiUserInfo <- function(user_ids, access_token) {
  user_ids <- paste(user_ids, collapse = ',')
  fetched <- RJSONIO::fromJSON(paste0('https://api.vk.com/method/users.get?user_ids=', user_ids,'&fields=photo_id,verified,sex,bdate,city,country,home_town,has_photo,photo_100,has_mobile,contacts,site,education,universities,schools,status,last_seen,followers_count,common_count,occupation,relatives,relation,personal,connections,wall_comments,activities,interests,music,movies,tv,books,games,about,quotes,timezone,screen_name,maiden_name,is_friend,friend_status,career,military&v=5.64&access_token=', access_token))
  fetched <- fetched$response
  outp <- lapply(1:length(fetched), function(k) data.frame('id' = ifelse(is.null(fetched[[k]][['id']]), NA, as.character(fetched[[k]][['id']])),
                                                           'first_name' = ifelse(is.null(fetched[[k]][['first_name']]), NA, fetched[[k]][['first_name']]),
                                                           'last_name' = ifelse(is.null(fetched[[k]][['last_name']]), NA, fetched[[k]][['last_name']]),
                                                           'sex' = ifelse(is.null(fetched[[k]][['sex']]), NA, fetched[[k]][['sex']]),
                                                           'screen_name' = ifelse(is.null(fetched[[k]][['screen_name']]), NA, fetched[[k]][['screen_name']]),
                                                           'bdate' = ifelse(is.null(fetched[[k]][['bdate']]), NA, fetched[[k]][['bdate']]),
                                                           'city' = ifelse(is.null(fetched[[k]][['city']][['title']]), NA, fetched[[k]][['city']][['title']]),
                                                           'country' = ifelse(is.null(fetched[[k]][['country']][['title']]), NA, fetched[[k]][['country']][['title']]),
                                                           'relation' = ifelse(is.null(fetched[[k]][['relation']]), NA, fetched[[k]][['relation']]),
                                                           'relation_partner_id' = ifelse(is.null(fetched[[k]][['relation_partner']][['id']]), NA, fetched[[k]][['relation_partner']][['id']]),
                                                           'has_photo' = ifelse(is.null(fetched[[k]][['has_photo']]), NA, fetched[[k]][['has_photo']]),
                                                           'wall_comments' = ifelse(is.null(fetched[[k]][['wall_comments']]), NA, fetched[[k]][['wall_comments']]),
                                                           'photo_100' = ifelse(is.null(fetched[[k]][['photo_100']]), NA, fetched[[k]][['photo_100']]),
                                                           'has_mobile' = ifelse(is.null(fetched[[k]][['has_mobile']]), NA, fetched[[k]][['has_mobile']]),
                                                           'can_see_all_posts' = ifelse(is.null(fetched[[k]][['can_see_all_posts']]), NA, fetched[[k]][['can_see_all_posts']]),
                                                           'status' = ifelse(is.null(fetched[[k]][['status']]), NA, fetched[[k]][['status']]),
                                                           'last_seen' = ifelse(is.null(fetched[[k]][['last_seen']]), NA, fetched[[k]][['last_seen']]), stringsAsFactors = F)  )
  outp <- do.call('rbind', outp)
  outp <- store_universities(items = fetched, output = outp)
  outp <- store_jobs(items = fetched, output = outp)
  outp <- store_schools(items = fetched, output = outp)
  return(outp)
}


getUserFollowersNum <- function(user_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/users.getFollowers?user_id=', user_id,'&fields=sex,bdate,city,country,photo_100,lists,domain,has_mobile,contacts,connections,site,education,universities,schools,can_see_all_posts,status,last_seen,common_count,relation,relatives&v=5.64&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    return(fetched$response$count)
  }
}


getUserFollowersInfo <- function(user_id, access_token, num_universities = 1, num_schools = 1, num_jobs = 1) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/users.getFollowers?user_id=', user_id,'&fields=sex,bdate,city,country,photo_100,lists,domain,has_mobile,contacts,connections,site,education,universities,schools,can_see_all_posts,status,last_seen,common_count,relation,relatives&v=5.64&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    items <- fetched$response$items
    output <- data.frame('id' = items$id, 
                         'first_name' = items$first_name,
                         'last_name' = items$last_name, stringsAsFactors = F)
    if ('sex' %in% names(items)) {
      output$sex <- items$sex
      output$sex[output$sex == 0] <- NA
      output$sex[output$sex == 1] <- 'female'
      output$sex[output$sex == 2] <- 'male'
    }
    if ('screen_name' %in% names(items)) {
      output$screen_name <- items$screen_name
    }
    if ('bdate' %in% names(items)) {
      output$bdate <- items$bdate
    }
    if ('city' %in% names(items)) {
      output$city <- items$city$title
    }
    if ('country' %in% names(items)) {
      output$country <- items$country$title
    }
    if ('relation' %in% names(items)) {
      output$relation <- items$relation
    }
    if ('relation_partner' %in% names(items)) {
      output$relation_partner_id <- items$relation_partner$id
    }
    if ('has_photo' %in% names(items)) {
      output$has_photo <- items$has_photo
    }
    if ('photo_100' %in% names(items)) {
      output$photo_url <- items$photo_100
    }
    if ('has_mobile' %in% names(items)) {
      output$has_mobile <- items$has_mobile
    }
    if ('can_see_all_posts' %in% names(items)) {
      output$can_see_all_posts <- items$can_see_all_posts
    }
    if ('status' %in% names(items)) {
      output$status <- items$status
    }
    if ('last_seen' %in% names(items)) {
      output$last_seen <- items$last_seen$time
    }
    if ('universities' %in% names(items)) {
      output$number_universities <- sapply(1:nrow(output), function(k) ifelse(is.null(items$universities[[k]]), NA,  ifelse(length(items$universities[[k]]) == 0, NA, nrow(items$universities[[k]])) ))
      there_are_universities <- which(!is.na(output$number_universities))
      universities <- items$universities
      for (pos in there_are_universities) {
        if ('from' %in% names(universities[[pos]])) {
          universities[[pos]] <- universities[[pos]][order(universities[[pos]]$from, decreasing = T),]
        }
      }
      output$university_last <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'name' %in% names(universities[[k]]), universities[[k]]$name[1], NA ) )
      output$university_last_id <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'id' %in% names(universities[[k]]), universities[[k]]$id[1], NA ) )
      output$university_last_end_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'graduation' %in% names(universities[[k]]), universities[[k]]$graduation[1], NA ) )
      output$university_last_city <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'city' %in% names(universities[[k]]), universities[[k]]$city[1], NA ) )
      output$university_last_country <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'country' %in% names(universities[[k]]), universities[[k]]$country[1], NA ) )
      output$university_last_department <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'faculty_name' %in% names(universities[[k]]), universities[[k]]$faculty_name[1], NA ) )
      output$university_last_education_form <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'education_form' %in% names(universities[[k]]), universities[[k]]$education_form[1], NA ) )
      output$university_last_degree <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'education_status' %in% names(universities[[k]]), universities[[k]]$education_status[1], NA ) )
      
      if (num_universities > 1) {
        for (i in 2:num_universities) {
          output[[paste0('university_', i, 'tolast')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'name' %in% names(universities[[k]]), universities[[k]]$name[i], NA ) )
          output[[paste0('university_', i, 'tolast_id')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'id' %in% names(universities[[k]]), universities[[k]]$id[i], NA ) )
          output[[paste0('university_', i, 'tolast_end_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'graduation' %in% names(universities[[k]]), universities[[k]]$graduation[i], NA ) )
          output[[paste0('university_', i, 'tolast_city')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'city' %in% names(universities[[k]]), universities[[k]]$city[i], NA ) )
          output[[paste0('university_', i, 'tolast_country')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'country' %in% names(universities[[k]]), universities[[k]]$country[i], NA ) )
          output[[paste0('university_', i, 'tolast_department')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'faculty_name' %in% names(universities[[k]]), universities[[k]]$faculty_name[1], NA ) )
          output[[paste0('university_', i, 'tolast_education_form')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'education_form' %in% names(universities[[k]]), universities[[k]]$education_form[i], NA ) )
          output[[paste0('university_', i, 'tolast_degree')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'education_status' %in% names(universities[[k]]), universities[[k]]$education_status[i], NA ) )
        }
      }
    }
    if ('schools' %in% names(items)) {
      output$number_schools <- sapply(1:nrow(output), function(k) ifelse(is.null(items$schools[[k]]), NA,  ifelse(length(items$schools[[k]]) == 0, NA, nrow(items$schools[[k]])) ))
      there_are_schools <- which(!is.na(output$number_schools))
      schools <- items$schools
      for (pos in there_are_schools) {
        if ('from' %in% names(schools[[pos]])) {
          schools[[pos]] <- schools[[pos]][order(schools[[pos]]$from, decreasing = T),]
        }
      }
      output$school_last <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'name' %in% names(schools[[k]]), schools[[k]]$name[1], NA ) )
      output$school_last_id <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'id' %in% names(schools[[k]]), schools[[k]]$id[1], NA ) )
      output$school_last_start_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'year_from' %in% names(schools[[k]]), schools[[k]]$year_from[1], NA ) )
      output$school_last_end_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'year_to' %in% names(schools[[k]]), schools[[k]]$year_to[1], NA ) )
      output$school_last_city <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'city' %in% names(schools[[k]]), schools[[k]]$city[1], NA ) )
      output$school_last_country <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'country' %in% names(schools[[k]]), schools[[k]]$country[1], NA ) )
      if (num_schools > 1) {
        for (i in 2:num_schools) {
          output[[paste0('school_', i, 'tolast')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'name' %in% names(schools[[k]]), schools[[k]]$name[i], NA ) )
          output[[paste0('school_', i, 'tolast_id')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'id' %in% names(schools[[k]]), schools[[k]]$id[i], NA ) )
          output[[paste0('school_', i, 'tolast_start_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'year_from' %in% names(schools[[k]]), schools[[k]]$year_from[i], NA ) )
          output[[paste0('school_', i, 'tolast_end_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'year_to' %in% names(schools[[k]]), schools[[k]]$year_to[i], NA ) )
          output[[paste0('school_', i, 'tolast_city')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'city' %in% names(schools[[k]]), schools[[k]]$city[i], NA ) )
          output[[paste0('school_', i, 'tolast_country')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'country' %in% names(schools[[k]]), schools[[k]]$country[i], NA ) )
        }
      }
    }
    if ('career' %in% names(items) ) {
      output$number_jobs <- sapply(1:nrow(output), function(k) ifelse(is.null(items$career[[k]]), NA,  ifelse(length(items$career[[k]]) == 0, NA, nrow(items$career[[k]])) ))
      there_are_jobs <- which(!is.na(output$number_jobs))
      jobs <- items$career
      for (pos in there_are_jobs) {
        if ('from' %in% names(jobs[[pos]])) {
          jobs[[pos]] <- jobs[[pos]][order(jobs[[pos]]$from, decreasing = T),]
        }
      }
      output$job_last <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'company' %in% names(jobs[[k]]), jobs[[k]]$company[1], NA ) )
      output$job_last_start_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'from' %in% names(jobs[[k]]), jobs[[k]]$from[1], NA ) )
      output$job_last_end_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'until' %in% names(jobs[[k]]), jobs[[k]]$until[1], NA ) )
      output$job_last_position <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'position' %in% names(jobs[[k]]), jobs[[k]]$position[1], NA ) )
      output$job_last_city <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'city_id' %in% names(jobs[[k]]), jobs[[k]]$city_id[1], NA ) )
      output$job_last_country <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'country_id' %in% names(jobs[[k]]), jobs[[k]]$country_id[1], NA ) )
      if (num_jobs > 1) {
        for (i in 2:num_jobs) {
          output[[paste0('job_', i, 'tolast')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'company' %in% names(jobs[[k]]), jobs[[k]]$company[i], NA ) )
          output[[paste0('job_', i, 'tolast_start_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'from' %in% names(jobs[[k]]), jobs[[k]]$from[i], NA ) )
          output[[paste0('job_', i, 'tolast_end_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'until' %in% names(jobs[[k]]), jobs[[k]]$until[i], NA ) )
          output[[paste0('job_', i, 'tolast_position')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'position' %in% names(jobs[[k]]), jobs[[k]]$position[i], NA ) )
          output[[paste0('job_', i, 'tolast_city')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'city_id' %in% names(jobs[[k]]), jobs[[k]]$city_id[i], NA ) )
          output[[paste0('job_', i, 'tolast_country')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'country_id' %in% names(jobs[[k]]), jobs[[k]]$country_id[i], NA ) )
        }
      }
    }
    return(output)
  }
}



getUserFriendsNum <- function(user_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/friends.get?user_id=', user_id,'&fields=nickname&v=5.64&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    return(fetched$response$count)
  }
}


getUserFriendsInfo <- function(user_id, access_token, num_universities = 1, num_schools = 1, num_jobs = 1) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/friends.get?user_id=', user_id,'&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&access_token=',access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    items <- fetched$response$items
    output <- data.frame('id' = items$id, 
                         'first_name' = items$first_name,
                         'last_name' = items$last_name, stringsAsFactors = F)
    if ('sex' %in% names(items)) {
      output$sex <- items$sex
      output$sex[output$sex == 0] <- NA
      output$sex[output$sex == 1] <- 'female'
      output$sex[output$sex == 2] <- 'male'
    }
    if ('screen_name' %in% names(items)) {
      output$screen_name <- items$screen_name
    }
    if ('bdate' %in% names(items)) {
      output$bdate <- items$bdate
    }
    if ('city' %in% names(items)) {
      output$city <- items$city$title
    }
    if ('country' %in% names(items)) {
      output$country <- items$country$title
    }
    if ('relation' %in% names(items)) {
      output$relation <- items$relation
    }
    if ('relation_partner' %in% names(items)) {
      output$relation_partner_id <- items$relation_partner$id
    }
    if ('has_photo' %in% names(items)) {
      output$has_photo <- items$has_photo
    }
    if ('photo_100' %in% names(items)) {
      output$photo_url <- items$photo_100
    }
    if ('has_mobile' %in% names(items)) {
      output$has_mobile <- items$has_mobile
    }
    if ('can_see_all_posts' %in% names(items)) {
      output$can_see_all_posts <- items$can_see_all_posts
    }
    if ('status' %in% names(items)) {
      output$status <- items$status
    }
    if ('last_seen' %in% names(items)) {
      output$last_seen <- items$last_seen$time
    }
    if ('universities' %in% names(items)) {
      output$number_universities <- sapply(1:nrow(output), function(k) ifelse(is.null(items$universities[[k]]), NA,  ifelse(length(items$universities[[k]]) == 0, NA, nrow(items$universities[[k]])) ))
      there_are_universities <- which(!is.na(output$number_universities))
      universities <- items$universities
      for (pos in there_are_universities) {
        if ('from' %in% names(universities[[pos]])) {
          universities[[pos]] <- universities[[pos]][order(universities[[pos]]$from, decreasing = T),]
        }
      }
      output$university_last <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'name' %in% names(universities[[k]]), universities[[k]]$name[1], NA ) )
      output$university_last_id <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'id' %in% names(universities[[k]]), universities[[k]]$id[1], NA ) )
      output$university_last_end_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'graduation' %in% names(universities[[k]]), universities[[k]]$graduation[1], NA ) )
      output$university_last_city <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'city' %in% names(universities[[k]]), universities[[k]]$city[1], NA ) )
      output$university_last_country <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'country' %in% names(universities[[k]]), universities[[k]]$country[1], NA ) )
      output$university_last_department <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'faculty_name' %in% names(universities[[k]]), universities[[k]]$faculty_name[1], NA ) )
      output$university_last_education_form <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'education_form' %in% names(universities[[k]]), universities[[k]]$education_form[1], NA ) )
      output$university_last_degree <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'education_status' %in% names(universities[[k]]), universities[[k]]$education_status[1], NA ) )
      
      if (num_universities > 1) {
        for (i in 2:num_universities) {
          output[[paste0('university_', i, 'tolast')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'name' %in% names(universities[[k]]), universities[[k]]$name[i], NA ) )
          output[[paste0('university_', i, 'tolast_id')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'id' %in% names(universities[[k]]), universities[[k]]$id[i], NA ) )
          output[[paste0('university_', i, 'tolast_end_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'graduation' %in% names(universities[[k]]), universities[[k]]$graduation[i], NA ) )
          output[[paste0('university_', i, 'tolast_city')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'city' %in% names(universities[[k]]), universities[[k]]$city[i], NA ) )
          output[[paste0('university_', i, 'tolast_country')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'country' %in% names(universities[[k]]), universities[[k]]$country[i], NA ) )
          output[[paste0('university_', i, 'tolast_department')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'faculty_name' %in% names(universities[[k]]), universities[[k]]$faculty_name[1], NA ) )
          output[[paste0('university_', i, 'tolast_education_form')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'education_form' %in% names(universities[[k]]), universities[[k]]$education_form[i], NA ) )
          output[[paste0('university_', i, 'tolast_degree')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'education_status' %in% names(universities[[k]]), universities[[k]]$education_status[i], NA ) )
        }
      }
    }
    if ('schools' %in% names(items)) {
      output$number_schools <- sapply(1:nrow(output), function(k) ifelse(is.null(items$schools[[k]]), NA,  ifelse(length(items$schools[[k]]) == 0, NA, nrow(items$schools[[k]])) ))
      there_are_schools <- which(!is.na(output$number_schools))
      schools <- items$schools
      for (pos in there_are_schools) {
        if ('from' %in% names(schools[[pos]])) {
          schools[[pos]] <- schools[[pos]][order(schools[[pos]]$from, decreasing = T),]
        }
      }
      output$school_last <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'name' %in% names(schools[[k]]), schools[[k]]$name[1], NA ) )
      output$school_last_id <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'id' %in% names(schools[[k]]), schools[[k]]$id[1], NA ) )
      output$school_last_start_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'year_from' %in% names(schools[[k]]), schools[[k]]$year_from[1], NA ) )
      output$school_last_end_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'year_to' %in% names(schools[[k]]), schools[[k]]$year_to[1], NA ) )
      output$school_last_city <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'city' %in% names(schools[[k]]), schools[[k]]$city[1], NA ) )
      output$school_last_country <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'country' %in% names(schools[[k]]), schools[[k]]$country[1], NA ) )
      if (num_schools > 1) {
        for (i in 2:num_schools) {
          output[[paste0('school_', i, 'tolast')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'name' %in% names(schools[[k]]), schools[[k]]$name[i], NA ) )
          output[[paste0('school_', i, 'tolast_id')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'id' %in% names(schools[[k]]), schools[[k]]$id[i], NA ) )
          output[[paste0('school_', i, 'tolast_start_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'year_from' %in% names(schools[[k]]), schools[[k]]$year_from[i], NA ) )
          output[[paste0('school_', i, 'tolast_end_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'year_to' %in% names(schools[[k]]), schools[[k]]$year_to[i], NA ) )
          output[[paste0('school_', i, 'tolast_city')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'city' %in% names(schools[[k]]), schools[[k]]$city[i], NA ) )
          output[[paste0('school_', i, 'tolast_country')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'country' %in% names(schools[[k]]), schools[[k]]$country[i], NA ) )
        }
      }
    }
    if ('career' %in% names(items) ) {
      output$number_jobs <- sapply(1:nrow(output), function(k) ifelse(is.null(items$career[[k]]), NA,  ifelse(length(items$career[[k]]) == 0, NA, nrow(items$career[[k]])) ))
      there_are_jobs <- which(!is.na(output$number_jobs))
      jobs <- items$career
      for (pos in there_are_jobs) {
        if ('from' %in% names(jobs[[pos]])) {
          jobs[[pos]] <- jobs[[pos]][order(jobs[[pos]]$from, decreasing = T),]
        }
      }
      output$job_last <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'company' %in% names(jobs[[k]]), jobs[[k]]$company[1], NA ) )
      output$job_last_start_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'from' %in% names(jobs[[k]]), jobs[[k]]$from[1], NA ) )
      output$job_last_end_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'until' %in% names(jobs[[k]]), jobs[[k]]$until[1], NA ) )
      output$job_last_position <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'position' %in% names(jobs[[k]]), jobs[[k]]$position[1], NA ) )
      output$job_last_city <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'city_id' %in% names(jobs[[k]]), jobs[[k]]$city_id[1], NA ) )
      output$job_last_country <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'country_id' %in% names(jobs[[k]]), jobs[[k]]$country_id[1], NA ) )
      if (num_jobs > 1) {
        for (i in 2:num_jobs) {
          output[[paste0('job_', i, 'tolast')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'company' %in% names(jobs[[k]]), jobs[[k]]$company[i], NA ) )
          output[[paste0('job_', i, 'tolast_start_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'from' %in% names(jobs[[k]]), jobs[[k]]$from[i], NA ) )
          output[[paste0('job_', i, 'tolast_end_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'until' %in% names(jobs[[k]]), jobs[[k]]$until[i], NA ) )
          output[[paste0('job_', i, 'tolast_position')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'position' %in% names(jobs[[k]]), jobs[[k]]$position[i], NA ) )
          output[[paste0('job_', i, 'tolast_city')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'city_id' %in% names(jobs[[k]]), jobs[[k]]$city_id[i], NA ) )
          output[[paste0('job_', i, 'tolast_country')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'country_id' %in% names(jobs[[k]]), jobs[[k]]$country_id[i], NA ) )
        }
      }
    }
    return(output)
  }
}


getUserGroupsNum <- function(user_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/groups.get?user_id=', user_id,'&extended=0&v=5.64&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    return(fetched$response$count)
  }
}


getUserGroupsInfo <- function(user_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/groups.get?user_id=', user_id,'&extended=1&v=5.64&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    return(fetched$response$items)
  }
}


getUserWall <- function(user_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    items <- fetched$response$items
    output <- data.frame('id' = items$id, 
                         'date' = as.Date(as.POSIXct(items$date, origin="1970-01-01")),
                         'date_POSIXct' = items$date,
                         'text' = items$text,
                         'comments_count' = items$comments$count,
                         'likes_count' = items$likes$count,
                         'reposts_count' = items$reposts$count,
                         'reposted' = sapply(1:nrow(items), function(k) ifelse(is.null(items$copy_history[k][[1]]), 0, 1)),
                         'reposted_from_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items$copy_history[k][[1]]), NA, items$copy_history[k][[1]]$from_id)),
                         'reposted_original_date' = sapply(1:nrow(items), function(k) ifelse(is.null(items$copy_history[k][[1]]), NA, as.Date(as.POSIXct(items$copy_history[k][[1]]$date, origin="1970-01-01")))),
                         'reposted_original_date_POSIXct' = sapply(1:nrow(items), function(k) ifelse(is.null(items$copy_history[k][[1]]), NA, items$copy_history[k][[1]]$date)),
                         'attachment' = sapply(1:nrow(items), function(k) ifelse(!is.null(items$attachments[k][[1]]), 1, ifelse(is.null(unique(items$copy_history[k][[1]]$attachment[[1]]$type)), 0, 1   )  )),
                         'attached_photo_id' = NA,
                         'attached_photo_url' = NA,
                         'attached_photo_text' = NA,
                         'attached_link_url' = NA,
                         'attached_link_title' = NA,
                         'attached_audio_id' = NA,
                         'attached_audio_artist' = NA,
                         'attached_audio_title' = NA,
                         'attached_video_id' = NA,
                         'attached_video_title' = NA,
                         'attached_video_platform' = NA,
                         'attached_video_access_key' = NA,
                         'attached_doc_id' = NA,
                         'attached_doc_title' = NA,
                         'attached_doc_url' = NA,
                         'attached_doc_access_key' = NA,
                         stringsAsFactors = F)
    rep_text <- sapply(1:nrow(items), function(k) ifelse(is.null(items$copy_history[k][[1]]), NA, items$copy_history[k][[1]]$text))
    output$text[output$reposted == 1] <- rep_text[which(!is.na(rep_text))]
    # photos
    output$attached_photo_id[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$photo$id), NA, ifelse(length(items$attachments[k][[1]]$photo$id) == 1, unlist(items$attachments[k][[1]]$photo$id), list(items$attachments[k][[1]]$photo$id)) ) )
    output$attached_photo_id[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$photo$id), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$photo$id) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$photo$id), list(items$copy_history[k][[1]]$attachments[[1]]$photo$id[!is.na(items$copy_history[k][[1]]$attachments[[1]]$photo$id)])) ))
    output$attached_photo_url[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$photo$photo_604), NA, ifelse(length(items$attachments[k][[1]]$photo$photo_604) == 1, unlist(items$attachments[k][[1]]$photo$photo_604), list(items$attachments[k][[1]]$photo$photo_604)) ) )
    output$attached_photo_url[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$photo$photo_604), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$photo$photo_604) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$photo$photo_604), list(items$copy_history[k][[1]]$attachments[[1]]$photo$photo_604[!is.na(items$copy_history[k][[1]]$attachments[[1]]$photo$photo_604)])) ))
    output$attached_photo_text[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$photo$text), NA, ifelse(length(items$attachments[k][[1]]$photo$text) == 1, unlist(items$attachments[k][[1]]$photo$text), list(items$attachments[k][[1]]$photo$text)) ) )
    output$attached_photo_text[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$photo$text), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$photo$text) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$photo$text), list(items$copy_history[k][[1]]$attachments[[1]]$photo$text[!is.na(items$copy_history[k][[1]]$attachments[[1]]$photo$text)])) ))
    # links
    output$attached_link_url[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$link$url), NA, ifelse(length(items$attachments[k][[1]]$link$url) == 1, unlist(items$attachments[k][[1]]$link$url), list(items$attachments[k][[1]]$link$url)) ) )
    output$attached_link_url[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$link$url), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$link$url) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$link$url), list(items$copy_history[k][[1]]$attachments[[1]]$link$url[!is.na(items$copy_history[k][[1]]$attachments[[1]]$link$url)])) ))
    output$attached_link_title[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$link$title), NA, ifelse(length(items$attachments[k][[1]]$link$title) == 1, unlist(items$attachments[k][[1]]$link$title), list(items$attachments[k][[1]]$link$title)) ) )
    output$attached_link_title[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$link$title), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$link$title) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$link$title), list(items$copy_history[k][[1]]$attachments[[1]]$link$title[!is.na(items$copy_history[k][[1]]$attachments[[1]]$link$title)])) ))
    # audios
    output$attached_audio_id[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$audio$id), NA, ifelse(length(items$attachments[k][[1]]$audio$id) == 1, unlist(items$attachments[k][[1]]$audio$id), list(items$attachments[k][[1]]$audio$id)) ) )
    output$attached_audio_id[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$audio$id), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$audio$id) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$audio$id), list(items$copy_history[k][[1]]$attachments[[1]]$audio$id[!is.na(items$copy_history[k][[1]]$attachments[[1]]$audio$id)])) ))
    output$attached_audio_artist[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$audio$artist), NA, ifelse(length(items$attachments[k][[1]]$audio$artist) == 1, unlist(items$attachments[k][[1]]$audio$artist), list(items$attachments[k][[1]]$audio$artist)) ) )
    output$attached_audio_artist[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$audio$artist), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$audio$artist) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$audio$artist), list(items$copy_history[k][[1]]$attachments[[1]]$audio$artist[!is.na(items$copy_history[k][[1]]$attachments[[1]]$audio$artist)])) ))
    output$attached_audio_title[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$audio$title), NA, ifelse(length(items$attachments[k][[1]]$audio$title) == 1, unlist(items$attachments[k][[1]]$audio$title), list(items$attachments[k][[1]]$audio$title)) ) )
    output$attached_audio_title[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$audio$title), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$audio$title) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$audio$title), list(items$copy_history[k][[1]]$attachments[[1]]$audio$title[!is.na(items$copy_history[k][[1]]$attachments[[1]]$audio$title)])) ))
    # videos
    output$attached_video_id[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$video$id), NA, ifelse(length(items$attachments[k][[1]]$video$id) == 1, unlist(items$attachments[k][[1]]$video$id), list(items$attachments[k][[1]]$video$id)) ) )
    output$attached_video_id[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$video$id), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$video$id) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$video$id), list(items$copy_history[k][[1]]$attachments[[1]]$video$id[!is.na(items$copy_history[k][[1]]$attachments[[1]]$video$id)])) ))
    output$attached_video_title[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$video$title), NA, ifelse(length(items$attachments[k][[1]]$video$title) == 1, unlist(items$attachments[k][[1]]$video$title), list(items$attachments[k][[1]]$video$title)) ) )
    output$attached_video_title[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$video$title), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$video$title) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$video$title), list(items$copy_history[k][[1]]$attachments[[1]]$video$title[!is.na(items$copy_history[k][[1]]$attachments[[1]]$video$title)])) ))
    output$attached_video_platform[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$video$platform), NA, ifelse(length(items$attachments[k][[1]]$video$platform) == 1, unlist(items$attachments[k][[1]]$video$platform), list(items$attachments[k][[1]]$video$platform)) ) )
    output$attached_video_platform[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$video$platform), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$video$platform) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$video$platform), list(items$copy_history[k][[1]]$attachments[[1]]$video$platform[!is.na(items$copy_history[k][[1]]$attachments[[1]]$video$platform)])) ))
    output$attached_video_access_key[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$video$access_key), NA, ifelse(length(items$attachments[k][[1]]$video$access_key) == 1, unlist(items$attachments[k][[1]]$video$access_key), list(items$attachments[k][[1]]$video$access_key)) ) )
    output$attached_video_access_key[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$video$access_key), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$video$access_key) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$video$access_key), list(items$copy_history[k][[1]]$attachments[[1]]$video$access_key[!is.na(items$copy_history[k][[1]]$attachments[[1]]$video$access_key)])) ))
    # docs
    output$attached_doc_id[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$doc$id), NA, ifelse(length(items$attachments[k][[1]]$doc$id) == 1, unlist(items$attachments[k][[1]]$doc$id), list(items$attachments[k][[1]]$doc$id)) ) )
    output$attached_doc_id[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$doc$id), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$doc$id) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$doc$id), list(items$copy_history[k][[1]]$attachments[[1]]$doc$id[!is.na(items$copy_history[k][[1]]$attachments[[1]]$doc$id)])) ))
    output$attached_doc_title[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$doc$title), NA, ifelse(length(items$attachments[k][[1]]$doc$title) == 1, unlist(items$attachments[k][[1]]$doc$title), list(items$attachments[k][[1]]$doc$title)) ) )
    output$attached_doc_title[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$doc$title), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$doc$title) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$doc$title), list(items$copy_history[k][[1]]$attachments[[1]]$doc$title[!is.na(items$copy_history[k][[1]]$attachments[[1]]$doc$title)])) ))
    output$attached_doc_url[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$doc$url), NA, ifelse(length(items$attachments[k][[1]]$doc$url) == 1, unlist(items$attachments[k][[1]]$doc$url), list(items$attachments[k][[1]]$doc$url)) ) )
    output$attached_doc_url[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$doc$url), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$doc$url) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$doc$url), list(items$copy_history[k][[1]]$attachments[[1]]$doc$url[!is.na(items$copy_history[k][[1]]$attachments[[1]]$doc$url)])) ))
    output$attached_doc_access_key[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$doc$access_key), NA, ifelse(length(items$attachments[k][[1]]$doc$access_key) == 1, unlist(items$attachments[k][[1]]$doc$access_key), list(items$attachments[k][[1]]$doc$access_key)) ) )
    output$attached_doc_access_key[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$doc$access_key), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$doc$access_key) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$doc$access_key), list(items$copy_history[k][[1]]$attachments[[1]]$doc$access_key[!is.na(items$copy_history[k][[1]]$attachments[[1]]$doc$access_key)])) ))
    return(output)
  }
}


getUserPostComments <- function(user_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getComments?owner_id=', user_id,'&post_id=', post_id,'&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    items <- fetched$response$items
    output <- data.frame('id' = items$id,
                         'from_id' = items$from_id,
                         'date' = items$date,
                         'text' = items$text,
                         'likes_count' = items$likes$count,
                         'reply_to_user' = items$reply_to_user,
                         'reply_to_comment' = items$reply_to_comment, stringsAsFactors = F)
    if (nrow(output) == 0) {
      return(NULL)
    } else {
      return(output)
    }
  }
}


getUserPostLikes <- function(user_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/likes.getList?type=post&owner_id=', user_id,'&item_id=', post_id,'&count=1000&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    items <- fetched$response$items
    return(items)
  }
} 


getUserPostReposts <- function(user_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', user_id,'&post_id=', post_id,'&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    items <- fetched$response$items
    items <- items[,-which(names(items) == 'copy_history')]
    output <- data.frame('post_id' = items$id,
                         'user_id' = items$from_id, stringsAsFactors = F)
    if ('date' %in% names(items)) {
      output$date <- items$date
    }
    if ('text' %in% names(items)) {
      output$text <- items$text
    }
    if ('post_source' %in% names(items)) {
      if ('type' %in% names(items$post_source)) {
        output$source_type <- items$post_source$type
      }
      if ('platform' %in% names(items$post_source)) {
        output$device <- items$post_source$device
      }
      if ('url' %in% names(items$post_source)) {
        output$source_url <- items$post_source$url
      }
    }
    if ('comments' %in% names(items) & 'count' %in% names(items$comments)) {
      output$num_comments <- items$comments$count
    }
    if ('likes' %in% names(items) & 'count' %in% names(items$likes)) {
      output$num_likes <- items$likes$count
    }
    if ('reposts' %in% names(items) & 'count' %in% names(items$reposts)) {
      output$num_reposts <- items$reposts$count
    }
    if ('views' %in% names(items) & 'count' %in% names(items$views)) {
      output$num_views <- items$views$count
    }
    if (nrow(output) == 0) {
      return(NULL)
    } else {
      return(output)
    }
  }
}


#----------------- GROUPS -----------------#
getGroupMembers <- function(group_id, access_token, num_universities = 1, num_schools = 1, num_jobs = 1, count = 1000) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/groups.getMembers?group_id=', group_id,'&fields=sex,bdate,city,country,photo_100,lists,domain,has_mobile,contacts,connections,site,education,universities,schools,career,can_see_all_posts,status,last_seen,common_count,relation,relatives&v=5.64&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    items <- fetched$response$items
    output <- data.frame('id' = items$id,
                         'first_name' = items$first_name,
                         'last_name' = items$last_name,
                         stringsAsFactors = F)
    if ('sex' %in% names(items)) {
      output$sex <- items$sex
      output$sex[output$sex == 0] <- NA
      output$sex[output$sex == 1] <- 'female'
      output$sex[output$sex == 2] <- 'male'
    }
    if ('screen_name' %in% names(items)) {
      output$screen_name <- items$screen_name
    }
    if ('mobile_phone' %in% names(items)) {
      output$mobile_phone <- items$mobile_phone
    }
    if ('site' %in% names(items)) {
      output$site <- items$site
    }
    if ('status' %in% names(items)) {
      output$status <- items$status
    }
    if ('common_count' %in% names(items)) {
      output$common_count <- items$common_count
    }
    if ('deactivated' %in% names(items)) {
      output$deactivated <- items$deactivated
    }
    if ('instagram' %in% names(items)) {
      output$instagram <- items$instagram
    }
    if ('skype' %in% names(items)) {
      output$skype <- items$skype
    }
    if ('facebook' %in% names(items)) {
      output$facebook <- items$facebook
    }
    if ('facebook_name' %in% names(items)) {
      output$facebook_name <- items$facebook_name
    }
    if ('twitter' %in% names(items)) {
      output$twitter <- items$twitter
    }
    if ('livejournal' %in% names(items)) {
      output$livejournal <- items$livejournal
    }
    if ('screen_name' %in% names(items)) {
      output$screen_name <- items$screen_name
    }
    if ('bdate' %in% names(items)) {
      output$bdate <- items$bdate
    }
    if ('city' %in% names(items)) {
      output$city <- items$city$title
    }
    if ('country' %in% names(items)) {
      output$country <- items$country$title
    }
    if ('relation' %in% names(items)) {
      output$relation <- items$relation
    }
    if ('relation_partner' %in% names(items)) {
      output$relation_partner_id <- items$relation_partner$id
    }
    if ('has_photo' %in% names(items)) {
      output$has_photo <- items$has_photo
    }
    if ('wall_comments' %in% names(items)) {
      output$num_wall_comments <- items$wall_comments
    }
    if ('photo_100' %in% names(items)) {
      output$photo_url <- items$photo_100
    }
    if ('has_mobile' %in% names(items)) {
      output$has_mobile <- items$has_mobile
    }
    if ('can_see_all_posts' %in% names(items)) {
      output$can_see_all_posts <- items$can_see_all_posts
    }
    if ('status' %in% names(items)) {
      output$status <- items$status
    }
    if ('last_seen' %in% names(items)) {
      output$last_seen <- items$last_seen$time
    }
    if ('universities' %in% names(items)) {
      output$number_universities <- sapply(1:nrow(output), function(k) ifelse(is.null(items$universities[[k]]), NA,  ifelse(length(items$universities[[k]]) == 0, NA, nrow(items$universities[[k]])) ))
      there_are_universities <- which(!is.na(output$number_universities))
      universities <- items$universities
      for (pos in there_are_universities) {
        if ('from' %in% names(universities[[pos]])) {
          universities[[pos]] <- universities[[pos]][order(universities[[pos]]$from, decreasing = T),]
        }
      }
      output$university_last <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'name' %in% names(universities[[k]]), universities[[k]]$name[1], NA ) )
      output$university_last_id <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'id' %in% names(universities[[k]]), universities[[k]]$id[1], NA ) )
      output$university_last_end_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'graduation' %in% names(universities[[k]]), universities[[k]]$graduation[1], NA ) )
      output$university_last_city <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'city' %in% names(universities[[k]]), universities[[k]]$city[1], NA ) )
      output$university_last_country <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'country' %in% names(universities[[k]]), universities[[k]]$country[1], NA ) )
      output$university_last_department <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'faculty_name' %in% names(universities[[k]]), universities[[k]]$faculty_name[1], NA ) )
      output$university_last_education_form <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'education_form' %in% names(universities[[k]]), universities[[k]]$education_form[1], NA ) )
      output$university_last_degree <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'education_status' %in% names(universities[[k]]), universities[[k]]$education_status[1], NA ) )
      
      if (num_universities > 1) {
        for (i in 2:num_universities) {
          output[[paste0('university_', i, 'tolast')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'name' %in% names(universities[[k]]), universities[[k]]$name[i], NA ) )
          output[[paste0('university_', i, 'tolast_id')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'id' %in% names(universities[[k]]), universities[[k]]$id[i], NA ) )
          output[[paste0('university_', i, 'tolast_end_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'graduation' %in% names(universities[[k]]), universities[[k]]$graduation[i], NA ) )
          output[[paste0('university_', i, 'tolast_city')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'city' %in% names(universities[[k]]), universities[[k]]$city[i], NA ) )
          output[[paste0('university_', i, 'tolast_country')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'country' %in% names(universities[[k]]), universities[[k]]$country[i], NA ) )
          output[[paste0('university_', i, 'tolast_department')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'faculty_name' %in% names(universities[[k]]), universities[[k]]$faculty_name[1], NA ) )
          output[[paste0('university_', i, 'tolast_education_form')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'education_form' %in% names(universities[[k]]), universities[[k]]$education_form[i], NA ) )
          output[[paste0('university_', i, 'tolast_degree')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_universities & 'education_status' %in% names(universities[[k]]), universities[[k]]$education_status[i], NA ) )
        }
      }
    }
    if ('schools' %in% names(items)) {
      output$number_schools <- sapply(1:nrow(output), function(k) ifelse(is.null(items$schools[[k]]), NA,  ifelse(length(items$schools[[k]]) == 0, NA, nrow(items$schools[[k]])) ))
      there_are_schools <- which(!is.na(output$number_schools))
      schools <- items$schools
      for (pos in there_are_schools) {
        if ('from' %in% names(schools[[pos]])) {
          schools[[pos]] <- schools[[pos]][order(schools[[pos]]$from, decreasing = T),]
        }
      }
      output$school_last <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'name' %in% names(schools[[k]]), schools[[k]]$name[1], NA ) )
      output$school_last_id <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'id' %in% names(schools[[k]]), schools[[k]]$id[1], NA ) )
      output$school_last_start_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'year_from' %in% names(schools[[k]]), schools[[k]]$year_from[1], NA ) )
      output$school_last_end_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'year_to' %in% names(schools[[k]]), schools[[k]]$year_to[1], NA ) )
      output$school_last_city <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'city' %in% names(schools[[k]]), schools[[k]]$city[1], NA ) )
      output$school_last_country <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'country' %in% names(schools[[k]]), schools[[k]]$country[1], NA ) )
      if (num_schools > 1) {
        for (i in 2:num_schools) {
          output[[paste0('school_', i, 'tolast')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'name' %in% names(schools[[k]]), schools[[k]]$name[i], NA ) )
          output[[paste0('school_', i, 'tolast_id')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'id' %in% names(schools[[k]]), schools[[k]]$id[i], NA ) )
          output[[paste0('school_', i, 'tolast_start_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'year_from' %in% names(schools[[k]]), schools[[k]]$year_from[i], NA ) )
          output[[paste0('school_', i, 'tolast_end_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'year_to' %in% names(schools[[k]]), schools[[k]]$year_to[i], NA ) )
          output[[paste0('school_', i, 'tolast_city')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'city' %in% names(schools[[k]]), schools[[k]]$city[i], NA ) )
          output[[paste0('school_', i, 'tolast_country')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_schools & 'country' %in% names(schools[[k]]), schools[[k]]$country[i], NA ) )
        }
      }
    }
    if ('career' %in% names(items) ) {
      output$number_jobs <- sapply(1:nrow(output), function(k) ifelse(is.null(items$career[[k]]), NA,  ifelse(length(items$career[[k]]) == 0, NA, nrow(items$career[[k]])) ))
      there_are_jobs <- which(!is.na(output$number_jobs))
      jobs <- items$career
      for (pos in there_are_jobs) {
        if ('from' %in% names(jobs[[pos]])) {
          jobs[[pos]] <- jobs[[pos]][order(jobs[[pos]]$from, decreasing = T),]
        }
      }
      output$job_last <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'company' %in% names(jobs[[k]]), jobs[[k]]$company[1], NA ) )
      output$job_last_start_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'from' %in% names(jobs[[k]]), jobs[[k]]$from[1], NA ) )
      output$job_last_end_year <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'until' %in% names(jobs[[k]]), jobs[[k]]$until[1], NA ) )
      output$job_last_position <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'position' %in% names(jobs[[k]]), jobs[[k]]$position[1], NA ) )
      output$job_last_city <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'city_id' %in% names(jobs[[k]]), jobs[[k]]$city_id[1], NA ) )
      output$job_last_country <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'country_id' %in% names(jobs[[k]]), jobs[[k]]$country_id[1], NA ) )
      if (num_jobs > 1) {
        for (i in 2:num_jobs) {
          output[[paste0('job_', i, 'tolast')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'company' %in% names(jobs[[k]]), jobs[[k]]$company[i], NA ) )
          output[[paste0('job_', i, 'tolast_start_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'from' %in% names(jobs[[k]]), jobs[[k]]$from[i], NA ) )
          output[[paste0('job_', i, 'tolast_end_year')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'until' %in% names(jobs[[k]]), jobs[[k]]$until[i], NA ) )
          output[[paste0('job_', i, 'tolast_position')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'position' %in% names(jobs[[k]]), jobs[[k]]$position[i], NA ) )
          output[[paste0('job_', i, 'tolast_city')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'city_id' %in% names(jobs[[k]]), jobs[[k]]$city_id[i], NA ) )
          output[[paste0('job_', i, 'tolast_country')]] <- sapply(1:nrow(output), function(k) ifelse(k %in% there_are_jobs & 'country_id' %in% names(jobs[[k]]), jobs[[k]]$country_id[i], NA ) )
        }
      }
    }
    return(output)
  }
}


getGroupInfo <- function(group_id, access_token, links = TRUE) {
  if (links) {
    fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/groups.getById?group_ids=', group_id,'&fields=city,country,place,description,wiki_page,members_count,start_date,finish_date,can_see_all_posts,activity,status,links,fixed_post,verified,site,ban_info&v=5.64&access_token=', access_token))
    if ('error' %in% names(fetched)) {
      cat('ERROR: ', fetched$error$error_msg, '\n')
      return(NULL)
    } else {
      output <- data.frame('id' = fetched$response$id,
                           'name' = fetched$response$name,
                           'screen_name' = fetched$response$screen_name,
                           'is_closed' = fetched$response$is_closed,
                           'description' = fetched$response$description,
                           'members_count' = fetched$response$members_count,
                           'status' = fetched$response$status,
                           'fixed_post' = fetched$response$fixed_post,
                           'verified' = fetched$response$verified,
                           'photo_url' = fetched$response$photo_100,
                           stringsAsFactors = F)
      output$links = fetched$response$links
      return(output)
    }
  } else {
    fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/groups.getById?group_ids=', group_id,'&fields=city,country,place,description,wiki_page,members_count,start_date,finish_date,can_see_all_posts,activity,status,fixed_post,verified,site,ban_info&v=5.64&access_token=', access_token))
    if ('error' %in% names(fetched)) {
      cat('ERROR: ', fetched$error$error_msg, '\n')
    } else {
      output <- data.frame('id' = fetched$response$id,
                           'name' = fetched$response$name,
                           'screen_name' = fetched$response$screen_name,
                           'is_closed' = fetched$response$is_closed,
                           'description' = fetched$response$description,
                           'members_count' = fetched$response$members_count,
                           'activity' = fetched$response$activity,
                           'status' = fetched$response$status,
                           'fixed_post' = fetched$response$fixed_post,
                           'verified' = fetched$response$verified,
                           'photo_url' = fetched$response$photo_100,
                           stringsAsFactors = F)
      return(output)
    }
  }
}


getGroupWall <- function(group_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    items <- fetched$response$items
    output <- data.frame('id' = items$id, 
                         'date' = as.Date(as.POSIXct(items$date, origin="1970-01-01")),
                         'date_POSIXct' = items$date,
                         'text' = items$text,
                         'comments_count' = items$comments$count,
                         'likes_count' = items$likes$count,
                         'reposts_count' = items$reposts$count,
                         'reposted' = sapply(1:nrow(items), function(k) ifelse(is.null(items$copy_history[k][[1]]), 0, 1)),
                         'reposted_from_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items$copy_history[k][[1]]), NA, items$copy_history[k][[1]]$from_id)),
                         'reposted_original_date' = sapply(1:nrow(items), function(k) ifelse(is.null(items$copy_history[k][[1]]), NA, as.Date(as.POSIXct(items$copy_history[k][[1]]$date, origin="1970-01-01")))),
                         'reposted_original_date_POSIXct' = sapply(1:nrow(items), function(k) ifelse(is.null(items$copy_history[k][[1]]), NA, items$copy_history[k][[1]]$date)),
                         'attachment' = sapply(1:nrow(items), function(k) ifelse(!is.null(items$attachments[k][[1]]), 1, ifelse(is.null(unique(items$copy_history[k][[1]]$attachment[[1]]$type)), 0, 1   )  )),
                         'attached_photo_id' = NA,
                         'attached_photo_url' = NA,
                         'attached_photo_text' = NA,
                         'attached_link_url' = NA,
                         'attached_link_title' = NA,
                         'attached_audio_id' = NA,
                         'attached_audio_artist' = NA,
                         'attached_audio_title' = NA,
                         'attached_video_id' = NA,
                         'attached_video_title' = NA,
                         'attached_video_platform' = NA,
                         'attached_video_access_key' = NA,
                         'attached_doc_id' = NA,
                         'attached_doc_title' = NA,
                         'attached_doc_url' = NA,
                         'attached_doc_access_key' = NA,
                         stringsAsFactors = F)
    rep_text <- sapply(1:nrow(items), function(k) ifelse(is.null(items$copy_history[k][[1]]), NA, items$copy_history[k][[1]]$text))
    output$text[output$reposted == 1] <- rep_text[which(!is.na(rep_text))]
    # photos
    output$attached_photo_id[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$photo$id), NA, ifelse(length(items$attachments[k][[1]]$photo$id) == 1, unlist(items$attachments[k][[1]]$photo$id), list(items$attachments[k][[1]]$photo$id)) ) )
    output$attached_photo_id[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$photo$id), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$photo$id) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$photo$id), list(items$copy_history[k][[1]]$attachments[[1]]$photo$id[!is.na(items$copy_history[k][[1]]$attachments[[1]]$photo$id)])) ))
    output$attached_photo_url[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$photo$photo_604), NA, ifelse(length(items$attachments[k][[1]]$photo$photo_604) == 1, unlist(items$attachments[k][[1]]$photo$photo_604), list(items$attachments[k][[1]]$photo$photo_604)) ) )
    output$attached_photo_url[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$photo$photo_604), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$photo$photo_604) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$photo$photo_604), list(items$copy_history[k][[1]]$attachments[[1]]$photo$photo_604[!is.na(items$copy_history[k][[1]]$attachments[[1]]$photo$photo_604)])) ))
    output$attached_photo_text[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$photo$text), NA, ifelse(length(items$attachments[k][[1]]$photo$text) == 1, unlist(items$attachments[k][[1]]$photo$text), list(items$attachments[k][[1]]$photo$text)) ) )
    output$attached_photo_text[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$photo$text), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$photo$text) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$photo$text), list(items$copy_history[k][[1]]$attachments[[1]]$photo$text[!is.na(items$copy_history[k][[1]]$attachments[[1]]$photo$text)])) ))
    # links
    output$attached_link_url[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$link$url), NA, ifelse(length(items$attachments[k][[1]]$link$url) == 1, unlist(items$attachments[k][[1]]$link$url), list(items$attachments[k][[1]]$link$url)) ) )
    output$attached_link_url[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$link$url), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$link$url) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$link$url), list(items$copy_history[k][[1]]$attachments[[1]]$link$url[!is.na(items$copy_history[k][[1]]$attachments[[1]]$link$url)])) ))
    output$attached_link_title[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$link$title), NA, ifelse(length(items$attachments[k][[1]]$link$title) == 1, unlist(items$attachments[k][[1]]$link$title), list(items$attachments[k][[1]]$link$title)) ) )
    output$attached_link_title[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$link$title), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$link$title) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$link$title), list(items$copy_history[k][[1]]$attachments[[1]]$link$title[!is.na(items$copy_history[k][[1]]$attachments[[1]]$link$title)])) ))
    # audios
    output$attached_audio_id[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$audio$id), NA, ifelse(length(items$attachments[k][[1]]$audio$id) == 1, unlist(items$attachments[k][[1]]$audio$id), list(items$attachments[k][[1]]$audio$id)) ) )
    output$attached_audio_id[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$audio$id), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$audio$id) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$audio$id), list(items$copy_history[k][[1]]$attachments[[1]]$audio$id[!is.na(items$copy_history[k][[1]]$attachments[[1]]$audio$id)])) ))
    output$attached_audio_artist[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$audio$artist), NA, ifelse(length(items$attachments[k][[1]]$audio$artist) == 1, unlist(items$attachments[k][[1]]$audio$artist), list(items$attachments[k][[1]]$audio$artist)) ) )
    output$attached_audio_artist[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$audio$artist), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$audio$artist) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$audio$artist), list(items$copy_history[k][[1]]$attachments[[1]]$audio$artist[!is.na(items$copy_history[k][[1]]$attachments[[1]]$audio$artist)])) ))
    output$attached_audio_title[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$audio$title), NA, ifelse(length(items$attachments[k][[1]]$audio$title) == 1, unlist(items$attachments[k][[1]]$audio$title), list(items$attachments[k][[1]]$audio$title)) ) )
    output$attached_audio_title[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$audio$title), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$audio$title) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$audio$title), list(items$copy_history[k][[1]]$attachments[[1]]$audio$title[!is.na(items$copy_history[k][[1]]$attachments[[1]]$audio$title)])) ))
    # videos
    output$attached_video_id[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$video$id), NA, ifelse(length(items$attachments[k][[1]]$video$id) == 1, unlist(items$attachments[k][[1]]$video$id), list(items$attachments[k][[1]]$video$id)) ) )
    output$attached_video_id[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$video$id), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$video$id) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$video$id), list(items$copy_history[k][[1]]$attachments[[1]]$video$id[!is.na(items$copy_history[k][[1]]$attachments[[1]]$video$id)])) ))
    output$attached_video_title[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$video$title), NA, ifelse(length(items$attachments[k][[1]]$video$title) == 1, unlist(items$attachments[k][[1]]$video$title), list(items$attachments[k][[1]]$video$title)) ) )
    output$attached_video_title[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$video$title), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$video$title) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$video$title), list(items$copy_history[k][[1]]$attachments[[1]]$video$title[!is.na(items$copy_history[k][[1]]$attachments[[1]]$video$title)])) ))
    output$attached_video_platform[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$video$platform), NA, ifelse(length(items$attachments[k][[1]]$video$platform) == 1, unlist(items$attachments[k][[1]]$video$platform), list(items$attachments[k][[1]]$video$platform)) ) )
    output$attached_video_platform[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$video$platform), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$video$platform) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$video$platform), list(items$copy_history[k][[1]]$attachments[[1]]$video$platform[!is.na(items$copy_history[k][[1]]$attachments[[1]]$video$platform)])) ))
    output$attached_video_access_key[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$video$access_key), NA, ifelse(length(items$attachments[k][[1]]$video$access_key) == 1, unlist(items$attachments[k][[1]]$video$access_key), list(items$attachments[k][[1]]$video$access_key)) ) )
    output$attached_video_access_key[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$video$access_key), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$video$access_key) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$video$access_key), list(items$copy_history[k][[1]]$attachments[[1]]$video$access_key[!is.na(items$copy_history[k][[1]]$attachments[[1]]$video$access_key)])) ))
    # docs
    output$attached_doc_id[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$doc$id), NA, ifelse(length(items$attachments[k][[1]]$doc$id) == 1, unlist(items$attachments[k][[1]]$doc$id), list(items$attachments[k][[1]]$doc$id)) ) )
    output$attached_doc_id[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$doc$id), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$doc$id) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$doc$id), list(items$copy_history[k][[1]]$attachments[[1]]$doc$id[!is.na(items$copy_history[k][[1]]$attachments[[1]]$doc$id)])) ))
    output$attached_doc_title[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$doc$title), NA, ifelse(length(items$attachments[k][[1]]$doc$title) == 1, unlist(items$attachments[k][[1]]$doc$title), list(items$attachments[k][[1]]$doc$title)) ) )
    output$attached_doc_title[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$doc$title), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$doc$title) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$doc$title), list(items$copy_history[k][[1]]$attachments[[1]]$doc$title[!is.na(items$copy_history[k][[1]]$attachments[[1]]$doc$title)])) ))
    output$attached_doc_url[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$doc$url), NA, ifelse(length(items$attachments[k][[1]]$doc$url) == 1, unlist(items$attachments[k][[1]]$doc$url), list(items$attachments[k][[1]]$doc$url)) ) )
    output$attached_doc_url[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$doc$url), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$doc$url) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$doc$url), list(items$copy_history[k][[1]]$attachments[[1]]$doc$url[!is.na(items$copy_history[k][[1]]$attachments[[1]]$doc$url)])) ))
    output$attached_doc_access_key[which(output$attachment == 1 & output$reposted == 0)] <- sapply(which(output$attachment == 1 & output$reposted == 0), function(k) ifelse(is.null(items$attachments[k][[1]]$doc$access_key), NA, ifelse(length(items$attachments[k][[1]]$doc$access_key) == 1, unlist(items$attachments[k][[1]]$doc$access_key), list(items$attachments[k][[1]]$doc$access_key)) ) )
    output$attached_doc_access_key[which(output$attachment == 1 & output$reposted == 1)] <- sapply(which(output$attachment == 1 & output$reposted == 1), function(k) ifelse(is.null(items$copy_history[k][[1]]$attachments[[1]]$doc$access_key), NA, ifelse(length(items$copy_history[k][[1]]$attachments[[1]]$doc$access_key) == 1, unlist(items$copy_history[k][[1]]$attachments[[1]]$doc$access_key), list(items$copy_history[k][[1]]$attachments[[1]]$doc$access_key[!is.na(items$copy_history[k][[1]]$attachments[[1]]$doc$access_key)])) ))
    return(output)
  }
}


getGroupPostComments <- function(group_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getComments?owner_id=', -group_id,'&post_id=', post_id,'&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    items <- fetched$response$items
    output <- data.frame('id' = items$id,
                         'from_id' = items$from_id,
                         'date' = items$date,
                         'text' = items$text,
                         'likes_count' = items$likes$count,
                         'reply_to_user' = items$reply_to_user,
                         'reply_to_comment' = items$reply_to_comment, stringsAsFactors = F)
    if (nrow(output) == 0) {
      return(NULL)
    } else {
      return(output)
    }
  }
}


getGroupPostReposts <- function(group_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', -group_id,'&post_id=', post_id,'&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    items <- fetched$response$items
    items <- items[,-which(names(items) == 'copy_history')]
    output <- data.frame('post_id' = items$id,
                         'user_id' = items$from_id,
                         'date' = items$date,
                         'text' = items$text,
                         'device' = items$post_source$platform,
                         'num_comments' = items$comments$count,
                         'num_likes' = items$likes$count,
                         'num_reposts' = items$reposts$count,
                         'num_views' = items$views$count, stringsAsFactors = F)
    if (nrow(output) == 0) {
      return(NULL)
    } else {
      return(output)
    }
  }
}


getGroupPostLikes <- function(group_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/likes.getList?type=post&owner_id=', -group_id,'&item_id=', post_id,'&count=1000&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    items <- fetched$response$items
    return(items)
  }
} 


#------- Internal:
store_universities <- function(items, output) {
  output$universities_number <- sapply(1:length(items), function(k) ifelse( 'universities' %in% names(items[[k]]) & length(items[[k]][['universities']]) > 0, length(items[[k]]$universities), NA ))
  # univ - list of data.frame with data on universities, NA otherwise
  univ <- sapply(1:length(items), function(k) ifelse( 'universities' %in% names(items[[k]]) & length(items[[k]][['universities']]) > 0, list(items[[k]][['universities']]), data.frame('graduation' = NA)))
  univ[which(!is.na(univ))]  <- sapply(which(!is.na(univ)), function(k) as.data.frame(do.call('rbind', univ[[k]])))
  output$universities <- univ
  return(output)
}


store_jobs <- function(items, output) {
  output$jobs_number <- sapply(1:length(items), function(k) ifelse( 'career' %in% names(items[[k]]) & length(items[[k]][['career']]) > 0, length(items[[k]]$career), NA ))
  jobs <- sapply(1:length(items), function(k) ifelse( 'career' %in% names(items[[k]]) & length(items[[k]][['career']]) > 0, list(items[[k]][['career']]), data.frame('graduation' = NA)))
  jobs[which(!is.na(jobs))] <- lapply(which(!is.na(jobs)), function(k) data.frame('company' = sapply(1:length(jobs[[k]]), function(m) ifelse(!is.null(jobs[[k]][[m]]$company), jobs[[k]][[m]]$company, NA)),
                                                                                  'country_id' = sapply(1:length(jobs[[k]]), function(m) ifelse(!is.null(jobs[[k]][[m]]$country_id), jobs[[k]][[m]]$country_id, NA)),
                                                                                  'city_id' = sapply(1:length(jobs[[k]]), function(m) ifelse(!is.null(jobs[[k]][[m]]$city_id), jobs[[k]][[m]]$city_id, NA)),
                                                                                  'from' = sapply(1:length(jobs[[k]]), function(m) ifelse(!is.null(jobs[[k]][[m]]$from), jobs[[k]][[m]]$from, NA)),
                                                                                  'until' = sapply(1:length(jobs[[k]]), function(m) ifelse(!is.null(jobs[[k]][[m]]$until), jobs[[k]][[m]]$until, NA)),
                                                                                  'position' = sapply(1:length(jobs[[k]]), function(m) ifelse(!is.null(jobs[[k]][[m]]$position), jobs[[k]][[m]]$position, NA)), stringsAsFactors = F))
  output$jobs <- jobs
  return(output)
}


store_schools <- function(items, output) {
  output$schools_number <- sapply(1:length(items), function(k) ifelse( 'schools' %in% names(items[[k]]) & length(items[[k]][['schools']]) > 0, length(items[[k]]$schools), NA ))
  schools <- sapply(1:length(items), function(k) ifelse( 'schools' %in% names(items[[k]]) & length(items[[k]][['schools']]) > 0, list(items[[k]][['schools']]), data.frame('graduation' = NA)))
  schools[which(!is.na(schools))] <- lapply(which(!is.na(schools)), function(k) data.frame('id' = sapply(1:length(schools[[k]]), function(m) ifelse(!is.null(schools[[k]][[m]]$id), schools[[k]][[m]]$id, NA)),
                                                                                           'country_id' = sapply(1:length(schools[[k]]), function(m) ifelse(!is.null(schools[[k]][[m]]$country), schools[[k]][[m]]$country, NA)),
                                                                                           'city_id' = sapply(1:length(schools[[k]]), function(m) ifelse(!is.null(schools[[k]][[m]]$city), schools[[k]][[m]]$city, NA)),
                                                                                           'name' = sapply(1:length(schools[[k]]), function(m) ifelse(!is.null(schools[[k]][[m]]$name), schools[[k]][[m]]$name, NA)),
                                                                                           'class' = sapply(1:length(schools[[k]]), function(m) ifelse(!is.null(schools[[k]][[m]]$class), schools[[k]][[m]]$class, NA)),
                                                                                           'year_from' = sapply(1:length(schools[[k]]), function(m) ifelse(!is.null(schools[[k]][[m]]$year_from), schools[[k]][[m]]$year_from, NA)),
                                                                                           'year_to' = sapply(1:length(schools[[k]]), function(m) ifelse(!is.null(schools[[k]][[m]]$year_to), schools[[k]][[m]]$year_to, NA)), stringsAsFactors = F))
  output$schools <- schools
  return(output)
}


# Chance city and country codes into names: get all dataset, then covern city codes to city names for each country separately?
# Trace all likes etc to posts on the wall: likes.getList
# Merge comments into Wall? 

# Check: date and date_POSIXct
# Check: all functions return a data.frame with (if AAA %in% names(items)) check
# Why user_idS in getUserInfo?

# getUserInfo - not more than 49 users

# Add:
# wall.search: search post on a wall by a criterion
# groups.isMember
# getUserPostCommentLikes

