


#------- Internal:
store_universities <- function(items, output) {
  if (class(items) == 'list') {
    output$universities_number <- sapply(1:length(items), function(k) ifelse( 'universities' %in% names(items[[k]]) & length(items[[k]][['universities']]) > 0, length(items[[k]]$universities), NA ))
    univ <- sapply(1:length(items), function(k) ifelse( 'universities' %in% names(items[[k]]) & length(items[[k]][['universities']]) > 0, list(items[[k]][['universities']]), data.frame('graduation' = NA)))
    if (length(which(!is.na(univ))) > 0) {
      univ <- lapply(1:length(univ), 
                     function(k) ifelse(is.na(univ[[k]]), NA, lapply(1:length(univ[[k]]), 
                                                                     function(m) data.frame('id' = ifelse(is.null(univ[[k]][[m]][['id']]), NA, as.character(univ[[k]][[m]][['id']])),
                                                                                                                  'country' = ifelse(is.null(univ[[k]][[m]][['country']]), NA, as.character(univ[[k]][[m]][['country']])),
                                                                                                                  'city' = ifelse(is.null(univ[[k]][[m]][['city']]), NA, as.character(univ[[k]][[m]][['city']])),
                                                                                                                  'name' = ifelse(is.null(univ[[k]][[m]][['name']]), NA, as.character(univ[[k]][[m]][['name']])),
                                                                                                                  'faculty' = ifelse(is.null(univ[[k]][[m]][['faculty']]), NA, as.character(univ[[k]][[m]][['faculty']])),
                                                                                                                  'faculty_name' = ifelse(is.null(univ[[k]][[m]][['faculty_name']]), NA, as.character(univ[[k]][[m]][['faculty_name']])),
                                                                                                                  'chair' = ifelse(is.null(univ[[k]][[m]][['chair']]), NA, as.character(univ[[k]][[m]][['chair']])),
                                                                                                                  'chair_name' = ifelse(is.null(univ[[k]][[m]][['chair_name']]), NA, as.character(univ[[k]][[m]][['chair_name']])),
                                                                                                                  'graduation' = ifelse(is.null(univ[[k]][[m]][['graduation']]), NA, as.character(univ[[k]][[m]][['graduation']])),
                                                                                                                  'education_form' = ifelse(is.null(univ[[k]][[m]][['education_form']]), NA, as.character(univ[[k]][[m]][['education_form']])),
                                                                                                                  'education_status' = ifelse(is.null(univ[[k]][[m]][['education_status']]), NA, as.character(univ[[k]][[m]][['education_status']])),
                                                                                                                  stringsAsFactors = F) )))
    } else {
      univ <- do.call('rbind', univ)
    }
    output$universities <- univ
    return(output)
  } else {
    if (class(items) == 'data.frame') {
      output$universities_number <- sapply(1:nrow(items), function(k) ifelse( 'universities' %in% names(items) & nrow(items[['universities']][[k]]) > 0, nrow(items$universities[[k]]), NA ))
      univ <- sapply(1:nrow(items), function(k) ifelse( 'universities' %in% names(items) & nrow(items[['universities']][[k]]) > 0, list(items[['universities']][[k]]), data.frame('graduation' = NA)))
      output$universities <- univ
      return(output)
    } else {
      cat('ERROR: Fetched data in the wrong format')
      return(NULL)
    }
  }
}


store_jobs <- function(items, output) {
  if (class(items) == 'list') {
    output$jobs_number <- sapply(1:length(items), function(k) ifelse( 'career' %in% names(items[[k]]) & length(items[[k]][['career']]) > 0, length(items[[k]]$career), NA ))
    jobs <- sapply(1:length(items), function(k) ifelse( 'career' %in% names(items[[k]]) & length(items[[k]][['career']]) > 0, list(items[[k]][['career']]), data.frame('graduation' = NA)))
    if (length(which(!is.na(jobs))) > 0) {
      jobs <- lapply(1:length(jobs), 
                     function(k) ifelse(is.na(jobs[[k]]), NA, lapply(1:length(jobs[[k]]), 
                                                                     function(m) data.frame('company' = ifelse(is.null(jobs[[k]][[m]][['company']]), NA, as.character(jobs[[k]][[m]][['company']])),
                                                                                                                                  'country_id' = ifelse(is.null(jobs[[k]][[m]][['country_id']]), NA, as.character(jobs[[k]][[m]][['country_id']])),
                                                                                                                                  'city_id' = ifelse(is.null(jobs[[k]][[m]][['city_id']]), NA, as.character(jobs[[k]][[m]][['city_id']])),
                                                                                                                                  'from' = ifelse(is.null(jobs[[k]][[m]][['from']]), NA, as.character(jobs[[k]][[m]][['from']])),
                                                                                                                                  'until' = ifelse(is.null(jobs[[k]][[m]][['until']]), NA, as.character(jobs[[k]][[m]][['until']])),
                                                                                                                                  'position' = ifelse(is.null(jobs[[k]][[m]][['position']]), NA, as.character(jobs[[k]][[m]][['position']])),
                                                                                                                                  stringsAsFactors = F) )) )
    } else {
      jobs <- do.call('rbind', jobs)
    }
    output$career <- jobs
    return(output)
  } else {
    if (class(items) == 'data.frame') {
      output$jobs_number <- sapply(1:nrow(items), function(k) ifelse( 'career' %in% names(items) & nrow(items[['career']][[k]]) > 0, nrow(items$career[[k]]), NA ))
      jobs <- sapply(1:nrow(items), function(k) ifelse( 'career' %in% names(items) & nrow(items[['career']][[k]]) > 0, list(items[['career']][[k]]), data.frame('graduation' = NA)))
      output$career <- jobs
      return(output)
    } else {
      cat('ERROR: Fetched data in the wrong format')
      return(NULL)
    }
  }
}


store_schools <- function(items, output) {
  if (class(items) == 'list') {
    output$schools_number <- sapply(1:length(items), function(k) ifelse( 'schools' %in% names(items[[k]]) & length(items[[k]][['schools']]) > 0, length(items[[k]]$schools), NA ))
    schools <- sapply(1:length(items), function(k) ifelse( 'schools' %in% names(items[[k]]) & length(items[[k]][['schools']]) > 0, list(items[[k]][['schools']]), data.frame('graduation' = NA)))
    if (length(which(!is.na(schools))) > 0) {
      schools <- lapply(1:length(schools), 
                        function(k) ifelse(is.na(schools[[k]]), NA, lapply(1:length(schools[[k]]), 
                                                                           function(m) data.frame('id' = ifelse(is.null(schools[[k]][[m]][['id']]), NA, as.character(schools[[k]][[m]][['id']])),
                                                                                                                                              'country' = ifelse(is.null(schools[[k]][[m]][['country']]), NA, as.character(schools[[k]][[m]][['country']])),
                                                                                                                                              'city' = ifelse(is.null(schools[[k]][[m]][['city']]), NA, as.character(schools[[k]][[m]][['city']])),
                                                                                                                                              'name' = ifelse(is.null(schools[[k]][[m]][['name']]), NA, as.character(schools[[k]][[m]][['name']])),
                                                                                                                                              'year_from' = ifelse(is.null(schools[[k]][[m]][['year_from']]), NA, as.character(schools[[k]][[m]][['year_from']])),
                                                                                                                                              'year_to' = ifelse(is.null(schools[[k]][[m]][['year_to']]), NA, as.character(schools[[k]][[m]][['year_to']])),
                                                                                                                                              'year_graduated' = ifelse(is.null(schools[[k]][[m]][['year_graduated']]), NA, as.character(schools[[k]][[m]][['year_graduated']])),
                                                                                                                                              'class' = ifelse(is.null(schools[[k]][[m]][['class']]), NA, as.character(schools[[k]][[m]][['class']])),
                                                                                                                                              'speciality' = ifelse(is.null(schools[[k]][[m]][['speciality']]), NA, as.character(schools[[k]][[m]][['speciality']])), stringsAsFactors = F) )))
    } else {
      schools <- do.call('rbind', schools)
    }
    output$schools <- schools
    return(output)
  } else {
    if (class(items) == 'data.frame') {
      output$schools_number <- sapply(1:nrow(items), function(k) ifelse( 'schools' %in% names(items) & nrow(items[['schools']][[k]]) > 0, nrow(items$schools[[k]]), NA ))
      schools <- sapply(1:nrow(items), 
                        function(k) ifelse( 'schools' %in% names(items) & nrow(items[['schools']][[k]]) > 0, list(items[['schools']][[k]]), data.frame('graduation' = NA)))
      output$schools <- schools
      return(output)
    } else {
      cat('ERROR: Fetched data in the wrong format')
      return(NULL)
    }
  }
}


extract_wall <- function(user_id, access_token, offset) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&offset=', offset,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
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


#------- Authorization:
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
getUserInfo <- function(user_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/users.get?user_id=', user_id,'&fields=photo_id,verified,sex,bdate,city,country,home_town,has_photo,photo_100,has_mobile,contacts,site,education,universities,schools,status,last_seen,followers_count,common_count,occupation,relatives,relation,personal,connections,wall_comments,activities,interests,music,movies,tv,books,games,about,quotes,timezone,screen_name,maiden_name,is_friend,friend_status,career,military&v=5.68&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  }
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
    output$last_seen_date <- as.Date(as.POSIXct(output$last_seen, origin="1970-01-01"))
  }
  output <- store_universities(items = items, output = output)
  output <- store_schools(items = items, output = output)
  output <- store_jobs(items = items, output = output)
  return(output)
}


getMultiUserInfo <- function(user_ids, access_token) {
  user_ids <- paste(user_ids, collapse = ',')
  fetched <- RJSONIO::fromJSON(paste0('https://api.vk.com/method/users.get?user_ids=', user_ids,'&fields=photo_id,verified,sex,bdate,city,country,home_town,has_photo,photo_100,has_mobile,contacts,site,education,universities,schools,status,last_seen,followers_count,common_count,occupation,relatives,relation,personal,connections,wall_comments,activities,interests,music,movies,tv,books,games,about,quotes,timezone,screen_name,maiden_name,is_friend,friend_status,career,military&v=5.68&access_token=', access_token))
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
                                                           'last_seen' = ifelse(is.null(fetched[[k]][['last_seen']]), NA, fetched[[k]][['last_seen']]),
                                                           stringsAsFactors = F)  )
  outp <- do.call('rbind', outp)
  outp$last_seen_date <- as.Date(as.POSIXct(outp$last_seen, origin="1970-01-01"))
  outp <- store_universities(items = fetched, output = outp)
  outp <- store_jobs(items = fetched, output = outp)
  outp <- store_schools(items = fetched, output = outp)
  return(outp)
}


getUserFollowersNum <- function(user_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/users.getFollowers?user_id=', user_id,'&fields=sex,bdate,city,country,photo_100,lists,domain,has_mobile,contacts,connections,site,education,universities,schools,can_see_all_posts,status,last_seen,common_count,relation,relatives&v=5.68&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  }
  return(fetched$response$count)
}


getUserFollowersInfo <- function(user_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/users.getFollowers?user_id=', user_id,'&count=1000&fields=sex,bdate,city,country,photo_100,lists,domain,has_mobile,contacts,connections,site,education,universities,schools,can_see_all_posts,status,last_seen,common_count,relation,relatives&v=5.68&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  } 
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
    output$last_seen_date <- as.Date(as.POSIXct(output$last_seen, origin="1970-01-01"))
  }
  output <- store_universities(items = items, output = output)
  output <- store_schools(items = items, output = output)
  output <- store_jobs(items = items, output = output)
  return(output)
}


getUserFriendsNum <- function(user_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/friends.get?user_id=', user_id,'&fields=nickname&v=5.68&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  } 
  return(fetched$response$count)
}


getUserFriendsInfo <- function(user_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/friends.get?user_id=', user_id,'&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&access_token=',access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  }
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
    output$last_seen_date <- as.Date(as.POSIXct(output$last_seen, origin="1970-01-01"))
  }
  output <- store_universities(items = items, output = output)
  output <- store_schools(items = items, output = output)
  output <- store_jobs(items = items, output = output)
  return(output)
}


getUserGroupsNum <- function(user_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/groups.get?user_id=', user_id,'&extended=0&v=5.68&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  } 
  return(fetched$response$count)
}


getUserGroupsInfo <- function(user_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/groups.get?user_id=', user_id,'&count=1000&extended=1&v=5.68&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  } 
  return(fetched$response$items)
}


getUserWall <- function(user_id, access_token, num_posts = 'all', verbose = FALSE) {
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    stop(wall$error$error_msg, '\n')
  } 
  if (num_posts != 'all') {
    if (suppressWarnings(is.na(as.numeric(num_posts)))) {
      stop('Wrong number of posts: num_posts must be numeric')
    }
  }
  avail_posts <- wall$response$count
  if (num_posts == 'all') {
    num_posts <- avail_posts
  } else {
    num_posts <- min(num_posts, avail_posts)
  }
  offsets <- 100 * 0:floor(num_posts/100)
  st <- proc.time()
  if (verbose) {
    cat('Iterations started.\n')
  }
  for (j in 1:length(offsets)) {
    if (j == 1) {
      total_output <- extract_wall(user_id = user_id, access_token = access_token, offset = offsets[j])
    } else {
      output <- extract_wall(user_id = user_id, access_token = access_token, offset = offsets[j])
      total_output <- rbind(total_output, output)
    }
    if (verbose) {
      cat('Iteration', j, '/', length(offsets),'done\n')
    }
    Sys.sleep(1)
  }
  fin <- proc.time()
  cat('Total time:', round(as.numeric((fin-st)[3]/60),3), 'minutes\n')
  return(total_output[1:num_posts,])
}


getUserPostComments <- function(user_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getComments?owner_id=', user_id,'&post_id=', post_id,'&count=100&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  } 
  items <- fetched$response$items
  if (is.null(nrow(items))) {
    return(NULL)
  } else{ 
    output <- data.frame('comment_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'id']), NA, items[k, 'id'])),
                         'commenter_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'from_id']), NA, items[k, 'from_id'])),
                         'date' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'date']), NA, items[k, 'date'])),
                         'text' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'text']), NA, items[k, 'text'])),
                         'likes_count' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'likes']), NA, as.numeric(items[k, 'likes']['count']))),
                         'reply_to_user' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reply_to_user']), NA, items[k, 'reply_to_user'])),
                         'reply_to_comment' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reply_to_comment']), NA, items[k, 'reply_to_comment'])), stringsAsFactors = F)
    output$user_id_wall <- user_id
    output$to_post_id <- post_id
    if (nrow(output) == 0) {
      return(NULL)
    } else {
      return(output)
    }
  }
}


getUserWallComments <- function(user_id, num_posts = 'all', access_token, verbose = FALSE) {
  #--- Get posts number and request the number of posts to retrieve
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    stop(wall$error$error_msg)
  }
  avail_posts <- wall$response$count
  if (avail_posts == 0) {
    stop('The user has no posts\n')
  } 
  if (num_posts == 'all') {
    num_posts <- avail_posts
  } else {
    if (suppressWarnings(is.na(as.numeric(num_posts))) | num_posts <= 0) {
      stop('Wrong number of posts: num_posts must be either "all" or numeric')
    }
  }
  num_posts <- min(num_posts, avail_posts)
  if (verbose) {
    cat(paste0('Retrieveing ', num_posts, ' requested post ids\n'))
  }
  if (num_posts <= 100) {
    all_requested_posts <- wall$response$items$id
    all_requested_posts <- all_requested_posts[1:num_posts]
  } else {
    all_requested_posts <- wall$response$items$id
    offsets <- 100 * 1:floor(num_posts/100)
    for (w in 1:length(offsets)) {
      wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&offset=', offsets[w],'&v=5.68&extended=0&access_token=', access_token))
      all_requested_posts <- c(all_requested_posts, wall$response$items$id)
    }
    all_requested_posts <- unique(all_requested_posts)
    all_requested_posts <- all_requested_posts[1:num_posts]
  }
  #--- Iteratively get comments for every requested post
  total_output <- list()
  st <- proc.time()
  if (verbose) {
    cat('Iterations started.\n')
  }
  for (j in 1:length(all_requested_posts)) {
    fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getComments?owner_id=', user_id,'&post_id=', all_requested_posts[j],'&count=100&need_likes=1&v=5.68&extended=0&access_token=', access_token))
    if ('error' %in% names(fetched)) {
      cat('ERROR in ', j, ':', fetched$error$error_msg, '\n')
      Sys.sleep(1)
      # return(NULL)
    } else {
      cur_total_count <- fetched$response$count
      newoffsets <- 100 * 0:floor(cur_total_count/100)
      items2_list <- list()
      for (ell in 1:length(newoffsets)) {
        fetched2 <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getComments?owner_id=', user_id,'&post_id=', all_requested_posts[j],'&count=100&offset=', newoffsets[ell],'&need_likes=1&v=5.68&extended=0&access_token=', access_token))
        items2 <- fetched2$response$items
        items2_list[[ell]] <- data.frame('comment_id' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'id']), NA, items2[k, 'id'])),
                                         'commenter_id' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'from_id']), NA, items2[k, 'from_id'])),
                                         'date' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'date']), NA, items2[k, 'date'])),
                                         'text' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'text']), NA, items2[k, 'text'])),
                                         'likes_count' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'likes']), NA, as.numeric(items2[k, 'likes']['count']))),
                                         'reply_to_user' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'reply_to_user']), NA, items2[k, 'reply_to_user'])),
                                         'reply_to_comment' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'reply_to_comment']), NA, items2[k, 'reply_to_comment'])), stringsAsFactors = F)
        items2_list[[ell]]$user_id_wall <- user_id
        items2_list[[ell]]$to_post_id <- all_requested_posts[j]
      }
      items_fin <- do.call('rbind', items2_list)
      total_output[[j]] <- items_fin
    }
    if (verbose) {
      cat('post', j, '(out of', num_posts, ') done\n')
    }
    Sys.sleep(1)
  } # end: for (j in 1:howmany)
  fin <- proc.time()
  cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
  total_output <- do.call('rbind', total_output)
  total_output$date <- as.Date(as.POSIXct(total_output$date, origin="1970-01-01"))
  return(total_output)
}


getUserPostLikes <- function(user_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/likes.getList?type=post&owner_id=', user_id,'&item_id=', post_id,'&count=1000&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  } 
  items <- fetched$response$items
  return(items)
} 


getUserWallLikes <- function(user_id, access_token, num_posts = 'all', verbose = FALSE) {
  #--- Get posts number and request the number of posts to retrieve
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    stop(wall$error$error_msg)
  }
  avail_posts <- wall$response$count
  if (avail_posts == 0) {
    stop('The user has no posts\n')
  } 
  if (num_posts == 'all') {
    num_posts <- avail_posts
  } else {
    if (suppressWarnings(is.na(as.numeric(num_posts))) | num_posts <= 0) {
      stop('Wrong number of posts: num_posts must be either "all" or numeric')
    }
  }
  num_posts <- min(num_posts, avail_posts)
  #--- Get all requested post ids
  if (verbose) {
    cat('Retrieveing all requested post ids\n')
  }
  if (num_posts <= 100) {
    all_requested_posts <- wall$response$items$id
    all_requested_posts <- all_requested_posts[1:num_posts]
  } else {
    all_requested_posts <- wall$response$items$id
    offsets <- 100 * 1:floor(num_posts/100)
    for (w in 1:length(offsets)) {
      wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&offset=', offsets[w],'&v=5.68&extended=0&access_token=', access_token))
      all_requested_posts <- c(all_requested_posts, wall$response$items$id)
    }
    all_requested_posts <- unique(all_requested_posts)
    all_requested_posts <- all_requested_posts[1:num_posts]
  }
  if (verbose) {
    cat('All requested post ids retrieved\n')
  }
  #--- Iteratively get likes for every requested post
  total_output <- list()
  st <- proc.time()
  if (verbose) {
    cat('Iterations started.\n')
  }
  for (j in 1:num_posts) {
    total_output[[j]] <- getUserPostLikes(user_id = user_id, post_id = all_requested_posts[j], access_token = access_token)
    names(total_output)[j] <- all_requested_posts[j]
    if (verbose) {
      cat('post', j, '(out of', num_posts, ') done\n')
    }
    Sys.sleep(1)
  }
  fin <- proc.time()
  cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
  return(total_output)
}


getUserMostLikingUsers <- function(user_id, access_token, num_posts = 'all', num_users = 'all', verbose = FALSE) {
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if (num_posts == 'all') {
    num_posts <- wall$response$count
  } else {
    if (suppressWarnings(is.na(as.numeric(num_posts))) | num_posts <= 0) {
      stop('Wrong number of posts: num_posts must be either "all" or numeric and positive')
    }
  }
  if (num_users != 'all' & (suppressWarnings(is.na(as.numeric(num_users))) | num_users <= 0)) {
    stop('Wrong number of users: num_users must be either "all" or numeric and positive')
  }
  likes <- getUserWallLikes(user_id = user_id, access_token = access_token, num_posts = num_posts, verbose = verbose)
  likes <- do.call('c', likes)
  if (length(likes) != 0) {
    tb <- table(likes)
    output <- data.frame('user_id' = names(tb), 'num_likes' = as.numeric(tb), stringsAsFactors = F)
    output <- output[order(output$num_likes, decreasing = T),]
    if (num_users != 'all') {
      output <- output[1:min(num_users, nrow(output)),]
    }
  } else {
    cat('There are no likes.\n')
    output <- NULL
  }
  return(output)
}


getUserPostReposts <- function(user_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', user_id,'&post_id=', post_id,'&count=1000&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  }
  if (is.null(nrow(fetched$response$items))) {
    return(NULL)
  } else{ 
    items <- fetched$response$items
    output <- data.frame('repost_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'id']), NA, items[k, 'id'])),
                         'reposter_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'from_id']), NA, items[k, 'from_id'])),
                         'reposted_user_id' = user_id,
                         'repost_of_post_id' = post_id,
                         'receiver_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'to_id']), NA, items[k, 'to_id'])),
                         'date' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'date']), NA, items[k, 'date'])),
                         'text' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'text']), NA, items[k, 'text'])),
                         'num_comments' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'comments']['count']), NA, as.numeric(items[k, 'comments']['count']))),
                         'num_likes' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'likes']['count']), NA, as.numeric(items[k, 'likes']['count']))), 
                         'num_reposts' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reposts']['count']), NA, as.numeric(items[k, 'reposts']['count']))), 
                         'num_views' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'views']['count']), NA, as.numeric(items[k, 'views']['count']))), stringsAsFactors = F)
    
    fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', user_id,'&post_id=', post_id,'&offset=', nrow(items)+1,'&count=1000&v=5.68&extended=0&access_token=', access_token))
    items2 <- fetched$response$items
    if (!is.null(nrow(items2))) {
      output2 <- data.frame('repost_id' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'id']), NA, items2[k, 'id'])),
                            'reposter_id' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'from_id']), NA, items2[k, 'from_id'])),
                            'reposted_user_id' = user_id,
                            'repost_of_post_id' = post_id,
                            'receiver_id' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'to_id']), NA, items2[k, 'to_id'])),
                            'date' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'date']), NA, items2[k, 'date'])),
                            'text' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'text']), NA, items2[k, 'text'])),
                            'num_comments' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'comments']['count']), NA, as.numeric(items2[k, 'comments']['count']))),
                            'num_likes' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'likes']['count']), NA, as.numeric(items2[k, 'likes']['count']))), 
                            'num_reposts' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'reposts']['count']), NA, as.numeric(items2[k, 'reposts']['count']))), 
                            'num_views' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'views']['count']), NA, as.numeric(items2[k, 'views']['count']))), stringsAsFactors = F)
      fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', user_id,'&post_id=', post_id,'&offset=', nrow(items) + nrow(items2) +1,'&count=1000&v=5.68&extended=0&access_token=', access_token))
      items3 <- fetched$response$items
      
      if (!is.null(nrow(items3))) {
        output3 <- data.frame('repost_id' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'id']), NA, items3[k, 'id'])),
                              'reposter_id' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'from_id']), NA, items3[k, 'from_id'])),
                              'reposted_user_id' = user_id,
                              'repost_of_post_id' = post_id,
                              'receiver_id' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'to_id']), NA, items3[k, 'to_id'])),
                              'date' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'date']), NA, items3[k, 'date'])),
                              'text' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'text']), NA, items3[k, 'text'])),
                              'num_comments' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'comments']['count']), NA, as.numeric(items3[k, 'comments']['count']))),
                              'num_likes' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'likes']['count']), NA, as.numeric(items3[k, 'likes']['count']))), 
                              'num_reposts' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'reposts']['count']), NA, as.numeric(items3[k, 'reposts']['count']))), 
                              'num_views' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'views']['count']), NA, as.numeric(items3[k, 'views']['count']))), stringsAsFactors = F)
      }
    }
    
    if (is.null(nrow(items2))) {
      output_fin <- output
    } else {
      if (is.null(nrow(items3))) {
        output_fin <- rbind(output, output2)
      } else {
        output_fin <- rbind(output, output2, output3)
      }
    }
    
    output_fin <- output_fin[!duplicated(output_fin),]
    
    if (nrow(output_fin) == 0) {
      return(NULL)
    } else {
      return(output_fin)
    }
  }
}


getUserWallReposts <- function(user_id, access_token, num_posts = 'all', verbose = FALSE) {
  #--- Get posts number and request the number of posts to retrieve
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=1000&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    stop(wall$error$error_msg)
  }
  avail_posts <- wall$response$count
  if (avail_posts == 0) {
    stop('The user has no posts\n')
  } 
  if (num_posts == 'all') {
    num_posts <- avail_posts
  } else {
    if (suppressWarnings(is.na(as.numeric(num_posts))) | num_posts <= 0) {
      stop('Wrong number of posts: num_posts must be either "all" or numeric')
    }
  }
  num_posts <- min(num_posts, avail_posts)
  #--- Get all requested post ids
  if (verbose) {
    cat('Retrieveing all requested post ids\n')
  }
  if (num_posts <= 100) {
    all_requested_posts <- wall$response$items$id
    all_requested_posts <- all_requested_posts[1:num_posts]
  } else {
    all_requested_posts <- wall$response$items$id
    offsets <- 100 * 1:floor(num_posts/100)
    for (w in 1:length(offsets)) {
      wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&offset=', offsets[w],'&v=5.68&extended=0&access_token=', access_token))
      all_requested_posts <- c(all_requested_posts, wall$response$items$id)
    }
    all_requested_posts <- unique(all_requested_posts)
    all_requested_posts <- all_requested_posts[1:num_posts]
  }
  if (verbose) {
    cat('All requested post ids retrieved\n')
  }
  #--- Iteratively get reposts for every requested post
  total_output <- list()
  st <- proc.time()
  if (verbose) {
    cat('Iterations started.\n')
  }
  for (j in 1:length(all_requested_posts)) {
    offset_count = 0
    fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', user_id,'&post_id=', all_requested_posts[j],'&count=1000&need_likes=1&v=5.68&extended=0&access_token=', access_token))
    if ('error' %in% names(fetched)) {
      cat('ERROR in ', j, ':', fetched$error$error_msg, '\n')
      Sys.sleep(1)
      # return(NULL)
    } else {
      items <- fetched$response$items
      items2_list <- list()
      items2_list[[1]] <- data.frame('repost_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'id']), NA, items[k, 'id'])),
                                     'reposter_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'from_id']), NA, items[k, 'from_id'])),
                                     'receiver_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'to_id']), NA, items[k, 'to_id'])),
                                     'date' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'date']), NA, items[k, 'date'])),
                                     'text' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'text']), NA, items[k, 'text'])),
                                     'num_comments' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'comments']['count']), NA, as.numeric(items[k, 'comments']['count']))),
                                     'num_likes' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'likes']['count']), NA, as.numeric(items[k, 'likes']['count']))), 
                                     'num_reposts' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reposts']['count']), NA, as.numeric(items[k, 'reposts']['count']))), 
                                     'num_views' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'views']['count']), NA, as.numeric(items[k, 'views']['count'])))
                                     , stringsAsFactors = F)
      items2_list[[1]]$user_id_wall <- user_id
      items2_list[[1]]$to_post_id <- all_requested_posts[j]
      
      while (nrow(fetched$response$items) == 1000) {
        # need to use offset
        offset_count = offset_count + 1
        fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', user_id,'&post_id=', all_requested_posts[j],'&count=1000&offset=', offset_count * 1000,'&need_likes=1&v=5.68&extended=0&access_token=', access_token))
        items <- fetched$response$items
        items2_list[[offset_count + 1]] <- data.frame('repost_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'id']), NA, items[k, 'id'])),
                                                      'reposter_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'from_id']), NA, items[k, 'from_id'])),
                                                      'receiver_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'to_id']), NA, items[k, 'to_id'])),
                                                      'date' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'date']), NA, items[k, 'date'])),
                                                      'text' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'text']), NA, items[k, 'text'])),
                                                      'num_comments' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'comments']['count']), NA, as.numeric(items[k, 'comments']['count']))),
                                                      'num_likes' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'likes']['count']), NA, as.numeric(items[k, 'likes']['count']))), 
                                                      'num_reposts' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reposts']['count']), NA, as.numeric(items[k, 'reposts']['count']))), 
                                                      'num_views' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'views']['count']), NA, as.numeric(items[k, 'views']['count'])))
                                                      , stringsAsFactors = F)
        items2_list[[offset_count + 1]]$user_id_wall <- user_id
        items2_list[[offset_count + 1]]$to_post_id <- all_requested_posts[j]
      } # end while 
      items_fin <- do.call('rbind', items2_list)
      total_output[[j]] <- items_fin
    } # end else not error
    if (verbose) {
      cat('post', j, '(out of', num_posts, ') done\n')
    }
    Sys.sleep(1)
  } # end j
  total_output <- do.call('rbind', total_output)
  fin <- proc.time()
  cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
  return(total_output)
}


getUserMostRepostingUsers <- function(user_id, access_token, num_posts = 'all', num_users = 'all', verbose = FALSE) {
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if (num_posts == 'all') {
    num_posts <- wall$response$count
  } else {
    if (suppressWarnings(is.na(as.numeric(num_posts))) | num_posts <= 0) {
      stop('Wrong number of posts: num_posts must be either "all" or numeric and positive')
    }
  }
  if (num_users != 'all' & (suppressWarnings(is.na(as.numeric(num_users))) | num_users <= 0)) {
    stop('Wrong number of users: num_users must be either "all" or numeric and positive')
  }
  repos <- getUserWallReposts(user_id = user_id, access_token = access_token, num_posts = num_posts, verbose = verbose)
  if (length(repos) != 0) {
    tb <- table(repos$reposter_id)
    output <- data.frame('user_id' = names(tb), 'num_reposts' = as.numeric(tb), stringsAsFactors = F)
    output <- output[order(output$num_reposts, decreasing = T),]
    if (num_users != 'all') {
      output <- output[1:min(num_users, nrow(output)),]
    }
  } else {
    cat('There are no reposts\n')
    output <- NULL
  }
  return(output)
}


getUserMostCommentingUsers <- function(user_id, access_token, num_posts = 'all', num_users = 'all', verbose = FALSE) {
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if (num_posts == 'all') {
    num_posts <- wall$response$count
  } else {
    if (suppressWarnings(is.na(as.numeric(num_posts))) | num_posts <= 0) {
      stop('Wrong number of posts: num_posts must be either "all" or numeric and positive')
    }
  }
  if (num_users != 'all' & (suppressWarnings(is.na(as.numeric(num_users))) | num_users <= 0)) {
    stop('Wrong number of users: num_users must be either "all" or numeric and positive')
  }
  coms <- getUserWallComments(user_id = user_id, access_token = access_token, num_posts = num_posts, verbose = verbose)
  if (length(coms) != 0) {
    tb <- table(coms$commenter_id)
    output <- data.frame('user_id' = names(tb), 'num_comments' = as.numeric(tb), stringsAsFactors = F)
    output <- output[order(output$num_comments, decreasing = T),]
    if (num_users != 'all') {
      output <- output[1:min(num_users, nrow(output)),]
    }
  } else {
    cat('There are no comments.\n')
    output <- NULL
  }
  return(output)
}


getUserWallSearchCount <- function(user_id, query, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.search?owner_id=', user_id,'&query=', query,'&count=100&v=5.68&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  } 
  return(fetched$response$count)
}


searchUserWall <- function(user_id, query, access_token, verbose = FALSE) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.search?owner_id=', user_id,'&query=', query,'&count=100&offset=0&v=5.68&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  }
  st <- proc.time()
  if (length(fetched$response$items) > 0) {
    output <- data.frame('object_id' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$id[k]), stringsAsFactors = F)
    
    if ('from_id' %in% names(fetched$response$items)) {
      output$from_user_id <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$from_id[k])
    }
    
    if ('owner_id' %in% names(fetched$response$items)) {
      output$group_id_wall <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$owner_id[k])
    }
    
    if ('date' %in% names(fetched$response$items)) {
      output$date <- as.Date(as.POSIXct(fetched$response$items$date, origin="1970-01-01"))
      output$date_POSIXct <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$date[k])
    }
    if ('post_type' %in% names(fetched$response$items)) {
      output$post_type <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$post_type[k])
    }
    if ('text' %in% names(fetched$response$items)) {
      output$text <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$text[k])
    }
    
    if ('comments' %in% names(fetched$response$items)) {
      output$comments_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$comments$count[k])
    }
    if ('likes' %in% names(fetched$response$items)) {
      output$likes_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$likes$count[k])
    }
    if ('reposts' %in% names(fetched$response$items)) {
      output$reposts_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$reposts$count[k])
    }
    if ('views' %in% names(fetched$response$items)) {
      output$views_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$views$count[k])
    }
  } else {
    output <- NULL
  }
  if (fetched$response$count > 100) {
    offsets <- 100 * seq(1, floor(fetched$response$count / 100))
    if (verbose) {
      cat(paste0('Iteration 1 (out of ', length(offsets)+1, ') done\n'))
    }
    
    m = 0
    for (off in offsets) {
      m <- m + 1
      fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.search?owner_id=', user_id,'&query=', query,'&count=100&offset=', off,'&v=5.68&access_token=', access_token))
      Sys.sleep(1)
      output2 <- data.frame('object_id' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$id[k]), stringsAsFactors = F)
      
      if ('from_id' %in% names(fetched$response$items)) {
        output2$from_user_id <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$from_id[k])
      }
      
      if ('owner_id' %in% names(fetched$response$items)) {
        output2$group_id_wall <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$owner_id[k])
      }
      
      if ('date' %in% names(fetched$response$items)) {
        output2$date <- as.Date(as.POSIXct(fetched$response$items$date, origin="1970-01-01"))
        output2$date_POSIXct <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$date[k])
      }
      if ('post_type' %in% names(fetched$response$items)) {
        output2$post_type <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$post_type[k])
      }
      if ('text' %in% names(fetched$response$items)) {
        output2$text <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$text[k])
      }
      
      if ('comments' %in% names(fetched$response$items)) {
        output2$comments_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$comments$count[k])
      }
      if ('likes' %in% names(fetched$response$items)) {
        output2$likes_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$likes$count[k])
      }
      if ('reposts' %in% names(fetched$response$items)) {
        output2$reposts_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$reposts$count[k])
      }
      if ('views' %in% names(fetched$response$items)) {
        output2$views_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$views$count[k])
      }
      output <- plyr::rbind.fill(output, output2)
      if (verbose) {
        cat(paste0('Iteration ', m+1,' (out of ', length(offsets)+1, ') done\n'))
      }
      Sys.sleep(1)
    }
  }
  fin <- proc.time()
  cat('Total time:', round(as.numeric((fin-st)[3]/60),3), 'minutes\n')
  return(output)
}


checkUsersAreMembers <- function(user_ids, group_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/groups.isMember?group_id=', group_id,'&user_ids=', paste(user_ids, collapse = ','),'&extended=1&v=5.68&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  } 
  return(fetched$response$member)
}


#----------------- GROUPS -----------------#
getGroupMembers <- function(group_id, access_token, num_users = 'all', verbose = FALSE) {
  st <- proc.time()
  info <- getGroupInfo(group_id = group_id, access_token = access_token)
  if (num_users == 'all') {
    howmany <- info$members_count
  } else {
    if (suppressWarnings(is.na(as.numeric(num_users))) | num_users <= 0) {
      stop('Wrong number of members to retrieve: num_users must be either "all" or numeric')
    } else {
      howmany <- min(info$members_count, num_users)
    }
  }
  offsets <- 1000 * 0:floor(max(howmany)/1000)
  for (j in 1:length(offsets)) {
    fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/groups.getMembers?group_id=', group_id,'&offset=', offsets[j],'&count=1000&&fields=sex,bdate,city,country,photo_100,lists,domain,has_mobile,contacts,connections,site,education,universities,schools,career,can_see_all_posts,status,last_seen,common_count,relation,relatives&v=5.68&access_token=', access_token))
    if ('error' %in% names(fetched)) {
      stop(fetched$error$error_msg)
    } else {
      items <- fetched$response$items
      output <- data.frame('id' = items$id, 'first_name' = items$first_name, 'last_name' = items$last_name, stringsAsFactors = F)
      
      output$sex <- sapply(1:nrow(output), function(k) ifelse(is.null(items$sex), NA, ifelse(items$sex[k] == 0, NA, ifelse(items$sex[k] == 1, 'female', 'male'))))
      output$screen_name <- sapply(1:nrow(output), function(k) ifelse(is.null(items$screen_name), NA, items$screen_name[k]))
      output$mobile_phone <- sapply(1:nrow(output), function(k) ifelse(is.null(items$mobile_phone), NA, items$mobile_phone[k]))
      output$site <- sapply(1:nrow(output), function(k) ifelse(is.null(items$site), NA, items$site[k]))
      output$status <- sapply(1:nrow(output), function(k) ifelse(is.null(items$status), NA, items$status[k]))
      output$common_count <- sapply(1:nrow(output), function(k) ifelse(is.null(items$common_count), NA, items$common_count[k]))
      output$deactivated <- sapply(1:nrow(output), function(k) ifelse(is.null(items$deactivated), NA, items$deactivated[k]))
      output$instagram <- sapply(1:nrow(output), function(k) ifelse(is.null(items$instagram), NA, items$instagram[k]))
      output$skype <- sapply(1:nrow(output), function(k) ifelse(is.null(items$skype), NA, items$skype[k]))
      output$facebook <- sapply(1:nrow(output), function(k) ifelse(is.null(items$facebook), NA, items$facebook[k]))
      output$facebook_name <- sapply(1:nrow(output), function(k) ifelse(is.null(items$facebook_name), NA, items$facebook_name[k]))
      output$twitter <- sapply(1:nrow(output), function(k) ifelse(is.null(items$twitter), NA, items$twitter[k]))
      output$livejournal <- sapply(1:nrow(output), function(k) ifelse(is.null(items$livejournal), NA, items$livejournal[k]))
      output$bdate <- sapply(1:nrow(output), function(k) ifelse(is.null(items$bdate), NA, items$bdate[k]))
      output$city <- sapply(1:nrow(output), function(k) ifelse(is.null(items$city), NA, items$city$title[k]))
      output$country <- sapply(1:nrow(output), function(k) ifelse(is.null(items$country), NA, items$country$title[k]))
      output$relation <- sapply(1:nrow(output), function(k) ifelse(is.null(items$relation), NA, items$relation[k]))
      output$relation_partner_id <- sapply(1:nrow(output), function(k) ifelse(is.null(items$relation_partner$id), NA, items$relation_partner$id[k]))
      output$has_photo <- sapply(1:nrow(output), function(k) ifelse(is.null(items$has_photo), NA, items$has_photo[k]))
      output$wall_comments <- sapply(1:nrow(output), function(k) ifelse(is.null(items$wall_comments), NA, items$wall_comments[k]))
      output$photo_url <- sapply(1:nrow(output), function(k) ifelse(is.null(items$photo_100), NA, items$photo_100[k]))
      output$has_mobile <- sapply(1:nrow(output), function(k) ifelse(is.null(items$has_mobile), NA, items$has_mobile[k]))
      output$can_see_all_posts <- sapply(1:nrow(output), function(k) ifelse(is.null(items$can_see_all_posts), NA, items$can_see_all_posts[k]))
      output$status <- sapply(1:nrow(output), function(k) ifelse(is.null(items$status), NA, items$status[k]))
      output$last_seen <- sapply(1:nrow(output), function(k) ifelse(is.null(items$last_seen), NA, items$last_seen$time[k]))
      output$last_seen_date <- as.Date(as.POSIXct(output$last_seen, origin="1970-01-01"))
      
      output <- store_universities(items = items, output = output)
      output <- store_schools(items = items, output = output)
      output <- store_jobs(items = items, output = output)
    }
    if (j == 1) {
      total_output <- output
    } else {
      total_output <- rbind(total_output, output)
    }
    if (verbose) {
      cat('Iteration', j, '/', length(offsets),'done\n')
    }
    Sys.sleep(1)
  }
  fin <- proc.time()
  cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
  return(total_output[1:howmany,])
}


getGroupInfo <- function(group_id, access_token, links = TRUE) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/groups.getById?group_ids=', group_id,'&fields=city,country,place,description,wiki_page,members_count,start_date,finish_date,can_see_all_posts,activity,status,links,fixed_post,verified,site,ban_info&v=5.68&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  } 
  output <- data.frame('id' = fetched$response$id, stringsAsFactors = F)
  if ('name' %in% names(fetched$response)) {
    output$name <- fetched$response$name
  }
  if ('screen_name' %in% names(fetched$response)) {
    output$screen_name <- fetched$response$screen_name
  }
  if ('is_closed' %in% names(fetched$response)) {
    output$is_closed <- fetched$response$is_closed
  }
  if ('description' %in% names(fetched$response)) {
    output$description <- fetched$response$description
  }
  if ('members_count' %in% names(fetched$response)) {
    output$members_count <- fetched$response$members_count
  }
  if ('status' %in% names(fetched$response)) {
    output$status <- fetched$response$status
  }
  if ('fixed_post' %in% names(fetched$response)) {
    output$fixed_post <- fetched$response$fixed_post
  }
  if ('verified' %in% names(fetched$response)) {
    output$verified <- fetched$response$verified
  }
  if ('photo_url' %in% names(fetched$response)) {
    output$photo_url <- fetched$response$photo_url
  }
  if (links) {
    if ('links' %in% names(fetched$response)) {
      output$links <- fetched$response$links
    }
  }
  return(output)
}


getGroupWall <- function(group_id, access_token, num_posts = 'all', verbose = FALSE) {
  if (num_posts!='all' & (suppressWarnings(is.na(as.numeric(group_id))) | group_id <= 0)) {
    stop('Error in group id: group_id can only be numeric and positive')
  }
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    stop(wall$error$error_msg, '\n')
  } 
  if (wall$response$count == 0) {
    stop('There are no posts on the wall')
  }
  if (num_posts == 'all') {
    num_posts <- wall$response$count
  } else {
    if (suppressWarnings(is.na(as.numeric(num_posts))) | num_posts <= 0) {
      stop('Wrong number of posts: num_posts must be either "all" or numeric and positive')
    } else {
      num_posts <- min(num_posts, wall$response$count)
    }
  } 
  offsets <- 100 * 0:floor(num_posts/100)
  st <- proc.time()
  if (verbose) {
    cat('Iterations started.\n')
  }
  for (j in 1:length(offsets)) {
    if (j == 1) {
      total_output <- extract_wall(user_id = -group_id, access_token = access_token, offset = offsets[j])
    } else {
      output <- extract_wall(user_id = -group_id, access_token = access_token, offset = offsets[j])
      total_output <- rbind(total_output, output)
    }
    if (verbose) {
      cat('Iteration', j, '/', length(offsets),'done\n')
    }
    Sys.sleep(1)
  }
  fin <- proc.time()
  cat('Total time:', round(as.numeric((fin-st)[3]/60),3), 'minutes\n')
  return(total_output[1:num_posts,])
}


getGroupPostComments <- function(group_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getComments?owner_id=', -group_id,'&post_id=', post_id,'&count=100&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  }
  items <- fetched$response$items
  if (is.null(nrow(items))) {
    return(NULL)
  } else{ 
    output <- data.frame('comment_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'id']), NA, items[k, 'id'])),
                         'commenter_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'from_id']), NA, items[k, 'from_id'])),
                         'date' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'date']), NA, items[k, 'date'])),
                         'text' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'text']), NA, items[k, 'text'])),
                         'likes_count' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'likes']), NA, as.numeric(items[k, 'likes']['count']))),
                         'reply_to_user' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reply_to_user']), NA, items[k, 'reply_to_user'])),
                         'reply_to_comment' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reply_to_comment']), NA, items[k, 'reply_to_comment'])), stringsAsFactors = F)
    output$user_id_wall <- group_id
    output$to_post_id <- post_id
    if (nrow(output) == 0) {
      return(NULL)
    } else {
      return(output)
    }
  }
}


getGroupWallComments <- function(group_id, num_posts = 'all', access_token, verbose = FALSE) {
  #--- Get posts number and request the number of posts to retrieve
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    stop(wall$error$error_msg)
  }
  avail_posts <- wall$response$count
  if (avail_posts == 0) {
    stop('The user has no posts\n')
  } 
  if (num_posts == 'all') {
    num_posts <- avail_posts
  } else {
    if (suppressWarnings(is.na(as.numeric(num_posts))) | num_posts <= 0) {
      stop('Wrong number of posts: num_posts must be either "all" or numeric')
    }
  }
  num_posts <- min(num_posts, avail_posts)
  if (verbose) {
    cat(paste0('Retrieveing ', num_posts, ' requested post ids\n'))
  }
  if (num_posts <= 100) {
    all_requested_posts <- wall$response$items$id
    all_requested_posts <- all_requested_posts[1:num_posts]
  } else {
    all_requested_posts <- wall$response$items$id
    offsets <- 100 * 1:floor(num_posts/100)
    for (w in 1:length(offsets)) {
      wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&offset=', offsets[w],'&v=5.68&extended=0&access_token=', access_token))
      all_requested_posts <- c(all_requested_posts, wall$response$items$id)
    }
    all_requested_posts <- unique(all_requested_posts)
    all_requested_posts <- all_requested_posts[1:num_posts]
  }
  #--- Iteratively get comments for every requested post
  total_output <- list()
  st <- proc.time()
  if (verbose) {
    cat('Iterations started.\n')
  }
  for (j in 1:length(all_requested_posts)) {
    fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getComments?owner_id=', -group_id,'&post_id=', all_requested_posts[j],'&count=100&need_likes=1&v=5.68&extended=0&access_token=', access_token))
    if ('error' %in% names(fetched)) {
      cat('ERROR in ', j, ':', fetched$error$error_msg, '\n')
      Sys.sleep(1)
      # return(NULL)
    } else {
      cur_total_count <- fetched$response$count
      newoffsets <- 100 * 0:floor(cur_total_count/100)
      items2_list <- list()
      ell = 1
      for (ell in 1:length(newoffsets)) {
        fetched2 <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getComments?owner_id=', -group_id,'&post_id=', all_requested_posts[j],'&count=100&offset=', newoffsets[ell],'&need_likes=1&v=5.68&extended=0&access_token=', access_token))
        items2 <- fetched2$response$items
        items2_list[[ell]] <- data.frame('comment_id' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'id']), NA, items2[k, 'id'])),
                                         'commenter_id' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'from_id']), NA, items2[k, 'from_id'])),
                                         'date' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'date']), NA, items2[k, 'date'])),
                                         'text' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'text']), NA, items2[k, 'text'])),
                                         'likes_count' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'likes']), NA, as.numeric(items2[k, 'likes']['count']))),
                                         'reply_to_user' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'reply_to_user']), NA, items2[k, 'reply_to_user'])),
                                         'reply_to_comment' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'reply_to_comment']), NA, items2[k, 'reply_to_comment'])), stringsAsFactors = F)
        items2_list[[ell]]$user_id_wall <- -group_id
        items2_list[[ell]]$to_post_id <- all_requested_posts[j]
      }
      items2 <- do.call('rbind', items2_list)
      total_output[[j]] <- items2
    }
    if (verbose) {
      cat('post', j, '(out of', num_posts, ') done\n')
    }
    Sys.sleep(1)
  } # end: for (j in 1:howmany)
  fin <- proc.time()
  cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
  total_output <- do.call('rbind', total_output)
  total_output$date <- as.Date(as.POSIXct(total_output$date, origin="1970-01-01"))
  return(total_output)
}


getGroupPostReposts <- function(group_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', -group_id,'&post_id=', post_id,'&count=1000&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  }
  if (is.null(nrow(fetched$response$items))) {
    return(NULL)
  } else{ 
    output <- data.frame('repost_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'id']), NA, items[k, 'id'])),
                         'reposter_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'from_id']), NA, items[k, 'from_id'])),
                         'reposted_user_id' = -group_id,
                         'repost_of_post_id' = post_id,
                         'receiver_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'to_id']), NA, items[k, 'to_id'])),
                         'date' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'date']), NA, items[k, 'date'])),
                         'text' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'text']), NA, items[k, 'text'])),
                         'num_comments' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'comments']['count']), NA, as.numeric(items[k, 'comments']['count']))),
                         'num_likes' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'likes']['count']), NA, as.numeric(items[k, 'likes']['count']))), 
                         'num_reposts' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reposts']['count']), NA, as.numeric(items[k, 'reposts']['count']))), 
                         'num_views' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'views']['count']), NA, as.numeric(items[k, 'views']['count']))), stringsAsFactors = F)
    
    items <- fetched$response$items
    fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', -group_id,'&post_id=', post_id,'&offset=', nrow(items),'&count=1000&v=5.68&extended=0&access_token=', access_token))
    items2 <- fetched$response$items
    if (!is.null(nrow(items2))) {
      output2 <- data.frame('repost_id' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'id']), NA, items2[k, 'id'])),
                            'reposter_id' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'from_id']), NA, items2[k, 'from_id'])),
                            'reposted_user_id' = -group_id,
                            'repost_of_post_id' = post_id,
                            'receiver_id' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'to_id']), NA, items2[k, 'to_id'])),
                            'date' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'date']), NA, items2[k, 'date'])),
                            'text' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'text']), NA, items2[k, 'text'])),
                            'num_comments' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'comments']['count']), NA, as.numeric(items2[k, 'comments']['count']))),
                            'num_likes' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'likes']['count']), NA, as.numeric(items2[k, 'likes']['count']))), 
                            'num_reposts' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'reposts']['count']), NA, as.numeric(items2[k, 'reposts']['count']))), 
                            'num_views' = sapply(1:nrow(items2), function(k) ifelse(is.null(items2[k, 'views']['count']), NA, as.numeric(items2[k, 'views']['count']))), stringsAsFactors = F)
      fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', -group_id,'&post_id=', post_id,'&offset=', nrow(items) + nrow(items2),'&count=1000&v=5.68&extended=0&access_token=', access_token))
      items3 <- fetched$response$items
      
      if (!is.null(nrow(items3))) {
        output3 <- data.frame('repost_id' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'id']), NA, items3[k, 'id'])),
                              'reposter_id' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'from_id']), NA, items3[k, 'from_id'])),
                              'reposted_user_id' = -group_id,
                              'repost_of_post_id' = post_id,
                              'receiver_id' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'to_id']), NA, items3[k, 'to_id'])),
                              'date' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'date']), NA, items3[k, 'date'])),
                              'text' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'text']), NA, items3[k, 'text'])),
                              'num_comments' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'comments']['count']), NA, as.numeric(items3[k, 'comments']['count']))),
                              'num_likes' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'likes']['count']), NA, as.numeric(items3[k, 'likes']['count']))), 
                              'num_reposts' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'reposts']['count']), NA, as.numeric(items3[k, 'reposts']['count']))), 
                              'num_views' = sapply(1:nrow(items3), function(k) ifelse(is.null(items3[k, 'views']['count']), NA, as.numeric(items3[k, 'views']['count']))), stringsAsFactors = F)
      }
    }
    
    if (is.null(nrow(items2))) {
      output_fin <- output
    } else {
      if (is.null(nrow(items3))) {
        output_fin <- rbind(output, output2)
      } else {
        output_fin <- rbind(output, output2, output3)
      }
    }
    
    output_fin <- output_fin[!duplicated(output_fin),]
    
    if (nrow(output_fin) == 0) {
      return(NULL)
    } else {
      return(output_fin)
    }
  }
}


getGroupWallReposts <- function(group_id, access_token, num_posts = 'all', verbose = FALSE) {
  #--- Get posts number and request the number of posts to retrieve
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=1000&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    stop(wall$error$error_msg)
  }
  avail_posts <- wall$response$count
  if (avail_posts == 0) {
    stop('The user has no posts\n')
  } 
  if (num_posts == 'all') {
    num_posts <- avail_posts
  } else {
    if (suppressWarnings(is.na(as.numeric(num_posts))) | num_posts <= 0) {
      stop('Wrong number of posts: num_posts must be either "all" or numeric')
    }
  }
  num_posts <- min(num_posts, avail_posts)
  #--- Get all requested post ids
  if (verbose) {
    cat('Retrieveing all requested post ids\n')
  }
  if (num_posts <= 100) {
    all_requested_posts <- wall$response$items$id
    all_requested_posts <- all_requested_posts[1:num_posts]
  } else {
    all_requested_posts <- wall$response$items$id
    offsets <- 100 * 1:floor(num_posts/100)
    for (w in 1:length(offsets)) {
      wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&offset=', offsets[w],'&v=5.68&extended=0&access_token=', access_token))
      all_requested_posts <- c(all_requested_posts, wall$response$items$id)
    }
    all_requested_posts <- unique(all_requested_posts)
    all_requested_posts <- all_requested_posts[1:num_posts]
  }
  if (verbose) {
    cat('All requested post ids retrieved\n')
  }
  #--- Iteratively get reposts for every requested post
  total_output <- list()
  st <- proc.time()
  if (verbose) {
    cat('Iterations started.\n')
  }
  for (j in 1:length(all_requested_posts)) {
    offset_count = 0
    fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', -group_id,'&post_id=', all_requested_posts[j],'&count=1000&need_likes=1&v=5.68&extended=0&access_token=', access_token))
    if ('error' %in% names(fetched)) {
      cat('ERROR in ', j, ':', fetched$error$error_msg, '\n')
      Sys.sleep(1)
      # return(NULL)
    } else {
      items <- fetched$response$items
      items2_list <- list()
      items2_list[[1]] <- data.frame('repost_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'id']), NA, items[k, 'id'])),
                                     'reposter_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'from_id']), NA, items[k, 'from_id'])),
                                     'receiver_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'to_id']), NA, items[k, 'to_id'])),
                                     'date' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'date']), NA, items[k, 'date'])),
                                     'text' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'text']), NA, items[k, 'text'])),
                                     'num_comments' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'comments']['count']), NA, as.numeric(items[k, 'comments']['count']))),
                                     'num_likes' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'likes']['count']), NA, as.numeric(items[k, 'likes']['count']))), 
                                     'num_reposts' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reposts']['count']), NA, as.numeric(items[k, 'reposts']['count']))), 
                                     'num_views' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'views']['count']), NA, as.numeric(items[k, 'views']['count'])))
                                     , stringsAsFactors = F)
      items2_list[[1]]$user_id_wall <- -group_id
      items2_list[[1]]$to_post_id <- all_requested_posts[j]
      
      while (nrow(fetched$response$items) == 1000) {
        # need to use offset
        offset_count = offset_count + 1
        fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', -group_id,'&post_id=', all_requested_posts[j],'&count=1000&offset=', offset_count * 1000,'&need_likes=1&v=5.68&extended=0&access_token=', access_token))
        items <- fetched$response$items
        items2_list[[offset_count + 1]] <- data.frame('repost_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'id']), NA, items[k, 'id'])),
                                                      'reposter_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'from_id']), NA, items[k, 'from_id'])),
                                                      'receiver_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'to_id']), NA, items[k, 'to_id'])),
                                                      'date' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'date']), NA, items[k, 'date'])),
                                                      'text' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'text']), NA, items[k, 'text'])),
                                                      'num_comments' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'comments']['count']), NA, as.numeric(items[k, 'comments']['count']))),
                                                      'num_likes' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'likes']['count']), NA, as.numeric(items[k, 'likes']['count']))), 
                                                      'num_reposts' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reposts']['count']), NA, as.numeric(items[k, 'reposts']['count']))), 
                                                      'num_views' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'views']['count']), NA, as.numeric(items[k, 'views']['count'])))
                                                      , stringsAsFactors = F)
        items2_list[[offset_count + 1]]$user_id_wall <- -group_id
        items2_list[[offset_count + 1]]$to_post_id <- all_requested_posts[j]
      } # end while 
      items_fin <- do.call('rbind', items2_list)
      total_output[[j]] <- items_fin
    } # end else not error
    if (verbose) {
      cat('post', j, '(out of', num_posts, ') done\n')
    }
    Sys.sleep(1)
  } # end j
  total_output <- do.call('rbind', total_output)
  fin <- proc.time()
  cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
  return(total_output)
}


getGroupPostLikes <- function(group_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/likes.getList?type=post&owner_id=', -group_id,'&item_id=', post_id,'&count=1000&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  }
  if (fetched$response$count <= 1000) {
    offsets = 0
  } else{
    offsets <- 1000 * 0:floor(fetched$response$count/1000)
  }
  output <- list()
  for (j in 1:length(offsets)) {
    output[[j]] <- jsonlite::fromJSON(paste0('https://api.vk.com/method/likes.getList?type=post&owner_id=', -group_id,'&item_id=', post_id,'&count=1000&offset=', offsets[j],'&v=5.64&extended=0&access_token=', access_token))$response$items
    Sys.sleep(1)
  }
  output <- unique(unlist(output))
  return(output)
} 


getGroupWallLikes <- function(group_id, access_token, num_posts = 'all', verbose = FALSE) {
  #--- Get posts number and request the number of posts to retrieve
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    stop(wall$error$error_msg)
  }
  avail_posts <- wall$response$count
  if (avail_posts == 0) {
    stop('The user has no posts\n')
  } 
  if (num_posts == 'all') {
    num_posts <- avail_posts
  } else {
    if (suppressWarnings(is.na(as.numeric(num_posts))) | num_posts <= 0) {
      stop('Wrong number of posts: num_posts must be either "all" or numeric and positive')
    }
  }
  num_posts <- min(num_posts, avail_posts)
  #--- Get all requested posts
  if (verbose) {
    cat('Retrieveing all requested posts\n')
  }
  if (num_posts <= 100) {
    all_requested_posts <- wall$response$items$id
  } else {
      all_requested_posts <- wall$response$items$id
      offsets <- 100 * 1:floor(num_posts/100)
      for (w in 1:length(offsets)) {
        wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&offset=', offsets[w],'&v=5.64&extended=0&access_token=', access_token))
        all_requested_posts <- c(all_requested_posts, wall$response$items$id)
      }
      all_requested_posts <- unique(all_requested_posts)
    }
  if (verbose) {
    cat('All requested posts retrieved\n')
  }
    #--- Iteratively get likes for every requested post
    total_output <- list()
    st <- proc.time()
    if (verbose) {
      cat('Iterations started.\n')
    }
    for (j in 1:num_posts) {
        total_output[[j]] <- getGroupPostLikes(group_id = group_id, post_id = all_requested_posts[j], access_token = access_token)
        names(total_output)[j] <- all_requested_posts[j]
        if (verbose) {
          cat('post', j, '(out of', num_posts, ') done\n')
        }
        Sys.sleep(1)
    }
    fin <- proc.time()
    cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
    return(total_output)
}


getGroupMostLikingUsers <- function(group_id, access_token, num_posts = 'all', num_users = 'all', verbose = FALSE) {
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if (num_posts == 'all') {
    num_posts <- wall$response$count
  } else {
    if (suppressWarnings(is.na(as.numeric(num_posts))) | num_posts <= 0) {
      stop('Wrong number of posts: num_posts must be either "all" or numeric and positive')
    }
  }
  if (num_users != 'all' & (suppressWarnings(is.na(as.numeric(num_users))) | num_users <= 0)) {
    stop('Wrong number of users: num_users must be either "all" or numeric and positive')
  }
  likes <- getGroupWallLikes(group_id = group_id, access_token = access_token, num_posts = num_posts, verbose = verbose)
  likes <- do.call('c', likes)
  if (length(likes) != 0) {
    tb <- table(likes)
    output <- data.frame('user_id' = names(tb), 'num_likes' = as.numeric(tb), stringsAsFactors = F)
    output <- output[order(output$num_likes, decreasing = T),]
    if (num_users != 'all') {
      output <- output[1:min(num_users, nrow(output)),]
    }
  } else {
    cat('There are no likes.\n')
    output <- NULL
  }
  return(output)
}


getGroupMostRepostingUsers <- function(group_id, access_token, num_posts = 'all', num_users = 'all', verbose = FALSE) {
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if (num_posts == 'all') {
    num_posts <- wall$response$count
  } else {
    if (suppressWarnings(is.na(as.numeric(num_posts))) | num_posts <= 0) {
      stop('Wrong number of posts: num_posts must be either "all" or numeric and positive')
    }
  }
  if (num_users != 'all' & (suppressWarnings(is.na(as.numeric(num_users))) | num_users <= 0)) {
    stop('Wrong number of users: num_users must be either "all" or numeric and positive')
  }
  repos <- getGroupWallReposts(group_id = group_id, access_token = access_token, num_posts = num_posts, verbose = verbose)
  if (length(repos) != 0) {
    tb <- table(repos$reposter_id)
    output <- data.frame('user_id' = names(tb), 'num_reposts' = as.numeric(tb), stringsAsFactors = F)
    output <- output[order(output$num_reposts, decreasing = T),]
    if (num_users != 'all') {
      output <- output[1:min(num_users, nrow(output)),]
    }
  } else {
    cat('There are no reposts\n')
    output <- NULL
  }
  return(output)
}


getGroupMostCommentingUsers <- function(group_id, access_token, num_posts = 'all', num_users = 'all', verbose = FALSE) {
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.68&extended=0&access_token=', access_token))
  if (num_posts == 'all') {
    num_posts <- wall$response$count
  } else {
    if (suppressWarnings(is.na(as.numeric(num_posts))) | num_posts <= 0) {
      stop('Wrong number of posts: num_posts must be either "all" or numeric and positive')
    }
  }
  if (num_users != 'all' & (suppressWarnings(is.na(as.numeric(num_users))) | num_users <= 0)) {
    stop('Wrong number of users: num_users must be either "all" or numeric and positive')
  }
  coms <- getGroupWallComments(group_id = group_id, access_token = access_token, num_posts = num_posts, verbose = verbose)
  if (length(coms) != 0) {
    tb <- table(coms$commenter_id)
    output <- data.frame('user_id' = names(tb), 'num_comments' = as.numeric(tb), stringsAsFactors = F)
    output <- output[order(output$num_comments, decreasing = T),]
    if (num_users != 'all') {
      output <- output[1:min(num_users, nrow(output)),]
    }
  } else {
    cat('There are no comments.\n')
    output <- NULL
  }
  return(output)
}


getGroupWallSearchCount <- function(group_id, query, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.search?owner_id=', -group_id,'&query=', query,'&count=100&v=5.68&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  } 
  return(fetched$response$count)
}


searchGroupWall <- function(group_id, query, access_token, verbose = FALSE) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.search?owner_id=', -group_id,'&query=', query,'&count=100&offset=0&v=5.68&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  }
  st <- proc.time()
  if (length(fetched$response$items) > 0) {
    output <- data.frame('object_id' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$id[k]), stringsAsFactors = F)
    
    if ('from_id' %in% names(fetched$response$items)) {
      output$from_user_id <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$from_id[k])
    }
    
    if ('owner_id' %in% names(fetched$response$items)) {
      output$group_id_wall <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$owner_id[k])
    }
    
    if ('date' %in% names(fetched$response$items)) {
      output$date <- as.Date(as.POSIXct(fetched$response$items$date, origin="1970-01-01"))
      output$date_POSIXct <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$date[k])
    }
    if ('post_type' %in% names(fetched$response$items)) {
      output$post_type <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$post_type[k])
    }
    if ('text' %in% names(fetched$response$items)) {
      output$text <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$text[k])
    }
    
    if ('comments' %in% names(fetched$response$items)) {
      output$comments_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$comments$count[k])
    }
    if ('likes' %in% names(fetched$response$items)) {
      output$likes_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$likes$count[k])
    }
    if ('reposts' %in% names(fetched$response$items)) {
      output$reposts_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$reposts$count[k])
    }
    if ('views' %in% names(fetched$response$items)) {
      output$views_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$views$count[k])
    }
  } else {
    output <- NULL
  }
  if (fetched$response$count > 100) {
    offsets <- 100 * seq(1, floor(fetched$response$count / 100))
    if (verbose) {
      cat(paste0('Iteration 1 (out of ', length(offsets)+1, ') done\n'))
    }
    
    m = 0
    for (off in offsets) {
      m <- m + 1
      fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.search?owner_id=', -group_id,'&query=', query,'&count=100&offset=', off,'&v=5.64&access_token=', access_token))
      Sys.sleep(1)
      output2 <- data.frame('object_id' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$id[k]), stringsAsFactors = F)
      
      if ('from_id' %in% names(fetched$response$items)) {
        output2$from_user_id <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$from_id[k])
      }
      
      if ('owner_id' %in% names(fetched$response$items)) {
        output2$group_id_wall <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$owner_id[k])
      }
      
      if ('date' %in% names(fetched$response$items)) {
        output2$date <- as.Date(as.POSIXct(fetched$response$items$date, origin="1970-01-01"))
        output2$date_POSIXct <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$date[k])
      }
      if ('post_type' %in% names(fetched$response$items)) {
        output2$post_type <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$post_type[k])
      }
      if ('text' %in% names(fetched$response$items)) {
        output2$text <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$text[k])
      }
      
      if ('comments' %in% names(fetched$response$items)) {
        output2$comments_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$comments$count[k])
      }
      if ('likes' %in% names(fetched$response$items)) {
        output2$likes_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$likes$count[k])
      }
      if ('reposts' %in% names(fetched$response$items)) {
        output2$reposts_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$reposts$count[k])
      }
      if ('views' %in% names(fetched$response$items)) {
        output2$views_count <- sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$views$count[k])
      }
      output <- plyr::rbind.fill(output, output2)
      if (verbose) {
        cat(paste0('Iteration ', m+1,' (out of ', length(offsets)+1, ') done\n'))
      }
      Sys.sleep(1)
    }
  }
  fin <- proc.time()
  cat('Total time:', round(as.numeric((fin-st)[3]/60),3), 'minutes\n')
  return(output)
}


searchNewsfeed <- function(query, access_token, start_time = NULL, end_time = NULL, verbose = FALSE) {
  total_output <- NULL
  if (is.null(start_time) | is.null(end_time)) {
    fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/newsfeed.search?q=', query,'&count=100&&access_token=', access_token))
  } else {
    start_time <- as.numeric(as.POSIXlt(start_time, tz = 'GMT'))
    end_time <- as.numeric(as.POSIXlt(end_time, tz = 'GMT'))
    fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/newsfeed.search?q=', query,'&start_time=', start_time,'&end_time=', end_time,'&count=100&access_token=', access_token))
  }
  if ('error' %in% names(fetched)) {
    stop(fetched$error$error_msg)
  }
  avail_posts <- fetched$response[[1]]
  offsets <- 100 * 0:floor(avail_posts/100)
  #cat(avail_posts)
  st <- proc.time()
  if (verbose) {
    cat('Total:', length(offsets) ,' iterations. Iterations started.\n')
  }
  for (j in 1:length(offsets)) {
    if (verbose) {
      cat('Iteration', j, '\n')
    }
    if (is.null(start_time) | is.null(end_time)) {
      fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/newsfeed.search?q=', query,'&offset=', offsets[j],'&count=100&access_token=', access_token))
    } else {
      fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/newsfeed.search?q=', query,'&offset=', offsets[j], '&start_time=', start_time,'&end_time=', end_time,'&count=100&access_token=', access_token))
    }
    if (j == 1 & length(fetched$response) > 1) {
      total_output <- lapply(2:length(fetched$response), function(k) data.frame('from_id' = fetched$response[[k]]$from_id, 'object_id' = fetched$response[[k]]$id,
                                                                                'post_type' = fetched$response[[k]]$post_type, 'text' = fetched$response[[k]]$text, 'date' = fetched$response[[k]]$date, 
                                                                                'photo_id' = ifelse(!is.null(fetched$response[[k]]$attachment$photo$pid), fetched$response[[k]]$attachment$photo$pid, NA),
                                                                                'photo_link' = ifelse(!is.null(fetched$response[[k]]$attachment$photo$src_xbig), fetched$response[[k]]$attachment$photo$src_xbig, NA),
                                                                                'photo_text' = ifelse(!is.null(fetched$response[[k]]$attachment$photo$text), fetched$response[[k]]$attachment$photo$text, NA),
                                                                                'link_url' = ifelse(!is.null(fetched$response[[k]]$attachment$link$url), fetched$response[[k]]$attachment$link$url, NA),
                                                                                'link_title' = ifelse(!is.null(fetched$response[[k]]$attachment$link$title), fetched$response[[k]]$attachment$link$title, NA),
                                                                                'link_image' = ifelse(!is.null(fetched$response[[k]]$attachment$link$image), fetched$response[[k]]$attachment$link$image, NA),
                                                                                'video_title' = ifelse(!is.null(fetched$response[[k]]$attachment$video$title), fetched$response[[k]]$attachment$video$title, NA),
                                                                                'video_description' = ifelse(!is.null(fetched$response[[k]]$attachment$video$description), fetched$response[[k]]$attachment$video$description, NA),
                                                                                'video_image' = ifelse(!is.null(fetched$response[[k]]$attachment$video$image_xbig), fetched$response[[k]]$attachment$video$image_xbig, NA),
                                                                                'video_id' = ifelse(!is.null(fetched$response[[k]]$attachment$video$vid), fetched$response[[k]]$attachment$video$vid, NA),
                                                                                stringsAsFactors = F))
      total_output <- do.call('rbind', total_output)
    } else {
      if (length(fetched$response) > 1) {
        output <- lapply(2:length(fetched$response), function(k) data.frame('from_id' = fetched$response[[k]]$from_id, 'object_id' = fetched$response[[k]]$id,
                                                                            'post_type' = fetched$response[[k]]$post_type, 'text' = fetched$response[[k]]$text, 'date' = fetched$response[[k]]$date, 
                                                                            'photo_id' = ifelse(!is.null(fetched$response[[k]]$attachment$photo$pid), fetched$response[[k]]$attachment$photo$pid, NA),
                                                                            'photo_link' = ifelse(!is.null(fetched$response[[k]]$attachment$photo$src_xbig), fetched$response[[k]]$attachment$photo$src_xbig, NA),
                                                                            'photo_text' = ifelse(!is.null(fetched$response[[k]]$attachment$photo$text), fetched$response[[k]]$attachment$photo$text, NA),
                                                                            'link_url' = ifelse(!is.null(fetched$response[[k]]$attachment$link$url), fetched$response[[k]]$attachment$link$url, NA),
                                                                            'link_title' = ifelse(!is.null(fetched$response[[k]]$attachment$link$title), fetched$response[[k]]$attachment$link$title, NA),
                                                                            'link_image' = ifelse(!is.null(fetched$response[[k]]$attachment$link$image), fetched$response[[k]]$attachment$link$image, NA),
                                                                            'video_title' = ifelse(!is.null(fetched$response[[k]]$attachment$video$title), fetched$response[[k]]$attachment$video$title, NA),
                                                                            'video_description' = ifelse(!is.null(fetched$response[[k]]$attachment$video$description), fetched$response[[k]]$attachment$video$description, NA),
                                                                            'video_image' = ifelse(!is.null(fetched$response[[k]]$attachment$video$image_xbig), fetched$response[[k]]$attachment$video$image_xbig, NA),
                                                                            'video_id' = ifelse(!is.null(fetched$response[[k]]$attachment$video$vid), fetched$response[[k]]$attachment$video$vid, NA),
                                                                            stringsAsFactors = F))
        output <- do.call('rbind', output)
        total_output <- rbind(total_output, output)
      }
    }
    Sys.sleep(1)
  } # j loop ends
  total_output$date <- as.Date(as.POSIXct(total_output$date, origin = "1970-01-01"))
  fin <- proc.time()
  cat('Total time:', round(as.numeric((fin-st)[3]/60),3), 'minutes\n')
  
  return(total_output)
}



