


#------- Internal:
store_universities <- function(items, output) {
  if (class(items) == 'list') {
    output$universities_number <- sapply(1:length(items), function(k) ifelse( 'universities' %in% names(items[[k]]) & length(items[[k]][['universities']]) > 0, length(items[[k]]$universities), NA ))
    univ <- sapply(1:length(items), function(k) ifelse( 'universities' %in% names(items[[k]]) & length(items[[k]][['universities']]) > 0, list(items[[k]][['universities']]), data.frame('graduation' = NA)))
    if (length(which(!is.na(univ))) > 0) {
      univ <- lapply(1:length(univ), function(k) ifelse(is.na(univ[[k]]), NA, lapply(1:length(univ[[k]]), function(m) data.frame('id' = ifelse(is.null(univ[[k]][[m]][['id']]), NA, as.character(univ[[k]][[m]][['id']])),
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
      jobs <- lapply(1:length(jobs), function(k) ifelse(is.na(jobs[[k]]), NA, lapply(1:length(jobs[[k]]), function(m) data.frame('company' = ifelse(is.null(jobs[[k]][[m]][['company']]), NA, as.character(jobs[[k]][[m]][['company']])),
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
      schools <- lapply(1:length(schools), function(k) ifelse(is.na(schools[[k]]), NA, lapply(1:length(schools[[k]]), function(m) data.frame('id' = ifelse(is.null(schools[[k]][[m]][['id']]), NA, as.character(schools[[k]][[m]][['id']])),
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
      schools <- sapply(1:nrow(items), function(k) ifelse( 'schools' %in% names(items) & nrow(items[['schools']][[k]]) > 0, list(items[['schools']][[k]]), data.frame('graduation' = NA)))
      output$schools <- schools
      return(output)
    } else {
      cat('ERROR: Fetched data in the wrong format')
      return(NULL)
    }
  }
}


extract_wall <- function(user_id, access_token, offset) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&offset=', offset,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
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
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/users.get?user_id=', user_id,'&fields=photo_id,verified,sex,bdate,city,country,home_town,has_photo,photo_100,has_mobile,contacts,site,education,universities,schools,status,last_seen,followers_count,common_count,occupation,relatives,relation,personal,connections,wall_comments,activities,interests,music,movies,tv,books,games,about,quotes,timezone,screen_name,maiden_name,is_friend,friend_status,career,military&v=5.64&access_token=', access_token))
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
      output$last_seen_date <- as.Date(as.POSIXct(output$last_seen, origin="1970-01-01"))
    }
    output <- store_universities(items = items, output = output)
    output <- store_schools(items = items, output = output)
    output <- store_jobs(items = items, output = output)
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
                                                           'last_seen' = ifelse(is.null(fetched[[k]][['last_seen']]), NA, fetched[[k]][['last_seen']]),
                                                           stringsAsFactors = F)  )
  outp$last_seen_date <- as.Date(as.POSIXct(outp$last_seen, origin="1970-01-01"))
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


getUserFollowersInfo <- function(user_id, access_token) {
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
      output$last_seen_date <- as.Date(as.POSIXct(output$last_seen, origin="1970-01-01"))
    }
    output <- store_universities(items = items, output = output)
    output <- store_schools(items = items, output = output)
    output <- store_jobs(items = items, output = output)
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


getUserFriendsInfo <- function(user_id, access_token) {
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
      output$last_seen_date <- as.Date(as.POSIXct(output$last_seen, origin="1970-01-01"))
    }
    output <- store_universities(items = items, output = output)
    output <- store_schools(items = items, output = output)
    output <- store_jobs(items = items, output = output)
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
    offsets <- 100 * 0:floor(fetched$response$count/100)
    if (length(offsets) > 1) {
      cat('VK API allows retrieving wall posts by chunks of 100 posts All posts are retrieved iteratively.\n')
      howmany <- suppressWarnings(ifelse(readline(prompt = paste0('There will be around ', length(offsets),' iterations. Do you want all of them? Type no if not: ')) == 'no', 
                                         as.numeric(readline(prompt = paste0('Type the number of iterations you want (<= ', length(offsets),'): '))), 
                                         length(offsets)))
      
      while (is.na(howmany)) {
        howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('Wrong input! Try again! Type the number of iterations you want (<= ', length(offsets),'): '))))
      }
    } else{
      howmany <- 1
    }
    st <- proc.time()
    cat('Iterations started.\n')
    for (j in 1:howmany) {
      if (j == 1) {
        total_output <- extract_wall(user_id = user_id, access_token = access_token, offset = offsets[j])
      } else {
        output <- extract_wall(user_id = user_id, access_token = access_token, offset = offsets[j])
        total_output <- rbind(total_output, output)
      }
      cat('Iteration', j, '/', howmany,'done\n')
      Sys.sleep(1)
    }
    fin <- proc.time()
    cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
    return(total_output)
  }
}


getUserPostComments <- function(user_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getComments?owner_id=', user_id,'&post_id=', post_id,'&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
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
}


getUserWallComments <- function(user_id, access_token, verbose = FALSE) {
  #--- Get posts number and request the number of posts to retrieve
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    cat('ERROR: ', wall$error$error_msg, '\n')
    return(NULL)
  } else {
    num_posts <- wall$response$count
    if (num_posts == 0) {
      cat('The user has no posts\n')
      return(NULL)
    } else {
      offsets <- 100 * 0:floor(wall$response$count/100)
      howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('There are ', num_posts,' posts. Type the number of recent posts you want to consider (<= ', num_posts,'): '))))
      howmany <- ifelse(howmany <= num_posts, howmany, num_posts)
      while (is.na(howmany)) {
        howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('Wrong input! Try again! Type the number of recent posts you want to consider (<= ', num_posts,'): '))))
      }
      #--- Iteratively get comments for every requested post
      total_output <- list()
      st <- proc.time()
      cat('Iterations started.\n')
      for (j in 1:howmany) {
        fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getComments?owner_id=', user_id,'&post_id=', wall$response$items$id[j],'&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
        if ('error' %in% names(fetched)) {
          cat('ERROR: ', fetched$error$error_msg, '\n')
          return(NULL)
        } else {
          items <- fetched$response$items
          if (!is.null(nrow(items))) {
            total_output[[j]] <- data.frame('comment_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'id']), NA, items[k, 'id'])),
                                            'commenter_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'from_id']), NA, items[k, 'from_id'])),
                                            'date' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'date']), NA, items[k, 'date'])),
                                            'text' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'text']), NA, items[k, 'text'])),
                                            'likes_count' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'likes']), NA, as.numeric(items[k, 'likes']['count']))),
                                            'reply_to_user' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reply_to_user']), NA, items[k, 'reply_to_user'])),
                                            'reply_to_comment' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reply_to_comment']), NA, items[k, 'reply_to_comment'])), stringsAsFactors = F)
            total_output[[j]]$user_id_wall <- user_id
            total_output[[j]]$to_post_id <- wall$response$items$id[j]
            if (verbose) {
              cat('post', j, '(out of', howmany, ') done\n')
            }
            Sys.sleep(1)
          }
        } # end: else
      } # end: for (j in 1:howmany)
      fin <- proc.time()
      cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
      total_output <- do.call('rbind', total_output)
      return(total_output)
    } # end: else below return(NULL)
  } # end: else below return(NULL) below ERROR
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


getUserWallLikes <- function(user_id, access_token, verbose = FALSE) {
  #--- Get posts number and request the number of posts to retrieve
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    cat('ERROR: ', wall$error$error_msg, '\n')
    return(NULL)
  } else {
    num_posts <- wall$response$count
    if (num_posts == 0) {
      cat('The user has no posts\n')
      return(NULL)
    } else {
      # offsets <- 100 * 0:floor(wall$response$count/100)
      howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('There are ', num_posts,' posts. Type the number of recent posts you want to consider (<= ', num_posts,'): '))))
      howmany <- ifelse(howmany <= num_posts, howmany, num_posts)
      while (is.na(howmany)) {
        howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('Wrong input! Try again! Type the number of recent posts you want to consider (<= ', num_posts,'): '))))
      }
      #--- Get all requested posts
      cat('Retrieveing all requested posts\n')
      if (howmany <= 100) {
        all_requested_posts <- wall$response$items$id
      } else {
        all_requested_posts <- wall$response$items$id
        offsets <- 100 * 1:floor(howmany/100)
        for (w in 1:length(offsets)) {
          wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&offset=', offsets[w],'&v=5.64&extended=0&access_token=', access_token))
          all_requested_posts <- c(all_requested_posts, wall$response$items$id)
        }
        all_requested_posts <- unique(all_requested_posts)
      }
      cat('All requested posts retrieved\n')
      #--- Iteratively get likes for every requested post
      total_output <- list()
      st <- proc.time()
      cat('Iterations started.\n')
      for (j in 1:howmany) {
        total_output[[j]] <- getUserPostLikes(user_id = user_id, post_id = all_requested_posts[j], access_token = access_token)
        names(total_output)[j] <- all_requested_posts[j]
        if (verbose) {
          cat('post', j, '(out of', howmany, ') done\n')
        }
        Sys.sleep(1)
      }
      fin <- proc.time()
      cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
      return(total_output)
    } # end: else below return(NULL)
  } # end: else below return(NULL) below ERROR
}


getUserMostLikingUsers <- function(user_id, access_token, num_users = 'all', verbose = FALSE) {
  if (num_users != 'all') {
    while (is.na(as.numeric(num_users))) {
      num_users <- suppressWarnings(readline(prompt = paste0('Wrong input! Try again! How many most liking users to retrieve? Type all or an integer: ')))
      if (num_users != 'all') {
        num_users <- as.numeric(num_users)
      }
    }
  }
  likes <- getUserWallLikes(user_id = user_id, access_token = access_token, verbose = verbose)
  likes <- do.call('c', likes)
  tb <- table(likes)
  output <- data.frame('user_id' = names(tb), 'num_likes' = as.numeric(tb), stringsAsFactors = F)
  output <- output[order(output$num_likes, decreasing = T),]
  if (num_users != 'all') {
    output <- output[1:num_users,]
  }
  return(output)
}


getUserPostReposts <- function(user_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', user_id,'&post_id=', post_id,'&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    if (is.null(nrow(items))) {
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
      if (nrow(output) == 0) {
        return(NULL)
      } else {
        return(output)
      }
    }
  }
}


getUserWallReposts <- function(user_id, access_token, verbose = FALSE) {
  #--- Get posts number and request the number of posts to retrieve
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', user_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    cat('ERROR: ', wall$error$error_msg, '\n')
    return(NULL)
  } else {
    num_posts <- wall$response$count
    if (num_posts == 0) {
      cat('The user has no posts\n')
      return(NULL)
    } else {
      offsets <- 100 * 0:floor(wall$response$count/100)
      howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('There are ', num_posts,' posts. Type the number of recent posts you want to consider (<= ', num_posts,'): '))))
      howmany <- ifelse(howmany <= num_posts, howmany, num_posts)
      while (is.na(howmany)) {
        howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('Wrong input! Try again! Type the number of recent posts you want to consider (<= ', num_posts,'): '))))
      }
      #--- Iteratively get reposts for every requested post
      total_output <- list()
      st <- proc.time()
      cat('Iterations started.\n')
      for (j in 1:howmany) {
        fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', user_id,'&post_id=', wall$response$items$id[j],'&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
        if ('error' %in% names(fetched)) {
          cat('ERROR: ', fetched$error$error_msg, '\n')
          return(NULL)
        } else {
          items <- fetched$response$items
          if (!is.null(nrow(items))) {
            total_output[[j]] <- data.frame('repost_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'id']), NA, items[k, 'id'])),
                                            'reposter_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'from_id']), NA, items[k, 'from_id'])),
                                            'receiver_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'to_id']), NA, items[k, 'to_id'])),
                                            'date' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'date']), NA, items[k, 'date'])),
                                            'text' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'text']), NA, items[k, 'text'])),
                                            'num_comments' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'comments']['count']), NA, as.numeric(items[k, 'comments']['count']))),
                                            'num_likes' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'likes']['count']), NA, as.numeric(items[k, 'likes']['count']))), 
                                            'num_reposts' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reposts']['count']), NA, as.numeric(items[k, 'reposts']['count']))), 
                                            'num_views' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'views']['count']), NA, as.numeric(items[k, 'views']['count'])))
                                            , stringsAsFactors = F)
            total_output[[j]]$reposted_user_id <- user_id
            total_output[[j]]$repost_of_post_id <- wall$response$items$id[j]
            if (verbose) {
              cat('post', j, '(out of', howmany, ') done\n')
            }
            Sys.sleep(1)
          }
        } # end: else
      } # end: for (j in 1:howmany)
      fin <- proc.time()
      cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
      total_output <- do.call('rbind', total_output)
      return(total_output)
    } # end: else below return(NULL)
  } # end: else below return(NULL) below ERROR
}


getUserWallSearchCount <- function(user_id, query, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.search?owner_id=', user_id,'&query=', query,'&count=2&v=5.64&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    return(fetched$response$count)
  }
}


searchUserWall <- function(user_id, query, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.search?owner_id=', user_id,'&query=', query,'&count=100&v=5.64&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    if (length(fetched$response$items) > 0) {
      output <- data.frame('object_id' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$id[k]),
                           'from_user_id' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$from_id[k]),
                           'user_id_wall' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$owner_id[k]),
                           'date' = as.Date(as.POSIXct(fetched$response$items$date, origin="1970-01-01")),
                           'date_POSIXct' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$date[k]),
                           'post_type' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$post_type[k]),
                           'to_post_id' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$post_id[k]),
                           'text' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$text[k]),
                           'comments_count' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$comments$count[k]),
                           'likes_count' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$likes$count[k]),
                           'reposts_count' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$reposts$count[k]),
                           'views_count' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$views$count[k]), stringsAsFactors = F)
    } else {
      output <- NULL
    }
    return(output)
  }
}



checkUsersAreMembers <- function(user_ids, group_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/groups.isMember?group_id=', group_id,'&user_ids=', paste(user_ids, collapse = ','),'&extended=1&v=5.64&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    return(fetched$response$member)
  }
}


#----------------- GROUPS -----------------#
getGroupMembers <- function(group_id, access_token, count = 1000) {
  st <- proc.time()
  info <- getGroupInfo(group_id = group_id, access_token = access_token)
  offsets <- 1000 * 0:floor(max(info$members_count)/1000)
  cat('VK API allows retrieving group members by chunks of 1000 users. All members are retrieved iteratively.\n')
  howmany <- suppressWarnings(ifelse(readline(prompt = paste0('There will be a total of ', length(offsets),' iterations. Do you want all of them? Type no if not: ')) == 'no', 
                                     as.numeric(readline(prompt = paste0('Type the number of iterations you want (<= ', length(offsets),'): '))), 
                                     length(offsets)))
  
  while (is.na(howmany)) {
    howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('Wrong input! Try again! Type the number of iterations you want (<= ', length(offsets),'): '))))
  }
  cat('Iterations started.\n')
  for (j in 1:howmany) {
    fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/groups.getMembers?group_id=', group_id,'&offset=', offsets[j],'&fields=sex,bdate,city,country,photo_100,lists,domain,has_mobile,contacts,connections,site,education,universities,schools,career,can_see_all_posts,status,last_seen,common_count,relation,relatives&v=5.64&access_token=', access_token))
    if ('error' %in% names(fetched)) {
      cat('ERROR: ', fetched$error$error_msg, '\n')
      return(NULL)
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
    cat('Iteration', j, '/', howmany,'done\n')
    Sys.sleep(1)
  }
  fin <- proc.time()
  cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
  return(total_output)
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
    offsets <- 100 * 0:floor(fetched$response$count/100)
    if (length(offsets) > 1) {
      cat('VK API allows retrieving wall posts by chunks of 100 posts All posts are retrieved iteratively.\n')
      howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('There will be around ', length(offsets),' iterations. Type the number of iterations you want (<= ', length(offsets),'): '))))
      howmany <- ifelse(howmany <= length(offsets), howmany, length(offsets))
      while (is.na(howmany)) {
        howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('Wrong input! Try again! Type the number of iterations you want (<= ', length(offsets),'): '))))
      }
    } else{
      howmany <- 1
    }
    st <- proc.time()
    cat('Iterations started.\n')
    for (j in 1:howmany) {
      if (j == 1) {
        total_output <- extract_wall(user_id = -group_id, access_token = access_token, offset = offsets[j])
      } else {
        output <- extract_wall(user_id = -group_id, access_token = access_token, offset = offsets[j])
        total_output <- rbind(total_output, output)
      }
      cat('Iteration', j, '/', howmany,'done\n')
      Sys.sleep(1)
    }
    fin <- proc.time()
    cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
    return(total_output)
  }
}


getGroupPostComments <- function(group_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getComments?owner_id=', -group_id,'&post_id=', post_id,'&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
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
}


getGroupWallComments <- function(group_id, access_token, verbose = FALSE) {
  #--- Get posts number and request the number of posts to retrieve
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    cat('ERROR: ', wall$error$error_msg, '\n')
    return(NULL)
  } else {
    num_posts <- wall$response$count
    if (num_posts == 0) {
      cat('The user has no posts\n')
      return(NULL)
    } else {
      offsets <- 100 * 0:floor(wall$response$count/100)
      howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('There are ', num_posts,' posts. Type the number of recent posts you want to consider (<= ', num_posts,'): '))))
      howmany <- ifelse(howmany <= num_posts, howmany, num_posts)
      while (is.na(howmany)) {
        howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('Wrong input! Try again! Type the number of recent posts you want to consider (<= ', num_posts,'): '))))
      }
      #--- Iteratively get comments for every requested post
      total_output <- list()
      st <- proc.time()
      cat('Iterations started.\n')
      for (j in 1:howmany) {
        fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getComments?owner_id=', -group_id,'&post_id=', wall$response$items$id[j],'&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
        if ('error' %in% names(fetched)) {
          cat('ERROR: ', fetched$error$error_msg, '\n')
          return(NULL)
        } else {
          items <- fetched$response$items
          if (!is.null(nrow(items))) {
            total_output[[j]] <- data.frame('comment_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'id']), NA, items[k, 'id'])),
                                            'commenter_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'from_id']), NA, items[k, 'from_id'])),
                                            'date' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'date']), NA, items[k, 'date'])),
                                            'text' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'text']), NA, items[k, 'text'])),
                                            'likes_count' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'likes']), NA, as.numeric(items[k, 'likes']['count']))),
                                            'reply_to_user' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reply_to_user']), NA, items[k, 'reply_to_user'])),
                                            'reply_to_comment' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reply_to_comment']), NA, items[k, 'reply_to_comment'])), stringsAsFactors = F)
            total_output[[j]]$user_id_wall <- group_id
            total_output[[j]]$to_post_id <- wall$response$items$id[j]
            if (verbose) {
              cat('post', j, '(out of', howmany, ') done\n')
            }
            Sys.sleep(1)
          }
        } # end: else
      } # end: for (j in 1:howmany)
      fin <- proc.time()
      cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
      total_output <- do.call('rbind', total_output)
      return(total_output)
    } # end: else below return(NULL)
  } # end: else below return(NULL) below ERROR
}


getGroupPostReposts <- function(group_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', -group_id,'&post_id=', post_id,'&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    if (is.null(nrow(items))) {
      return(NULL)
    } else{ 
      items <- fetched$response$items
      output <- data.frame('repost_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'id']), NA, items[k, 'id'])),
                           'reposter_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'from_id']), NA, items[k, 'from_id'])),
                           'reposted_user_id' = group_id,
                           'repost_of_post_id' = post_id,
                           'receiver_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'to_id']), NA, items[k, 'to_id'])),
                           'date' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'date']), NA, items[k, 'date'])),
                           'text' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'text']), NA, items[k, 'text'])),
                           'num_comments' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'comments']['count']), NA, as.numeric(items[k, 'comments']['count']))),
                           'num_likes' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'likes']['count']), NA, as.numeric(items[k, 'likes']['count']))), 
                           'num_reposts' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reposts']['count']), NA, as.numeric(items[k, 'reposts']['count']))), 
                           'num_views' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'views']['count']), NA, as.numeric(items[k, 'views']['count']))), stringsAsFactors = F)
      if (nrow(output) == 0) {
        return(NULL)
      } else {
        return(output)
      }
    }
  }
}


getGroupWallReposts <- function(group_id, access_token, verbose = FALSE) {
  #--- Get posts number and request the number of posts to retrieve
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    cat('ERROR: ', wall$error$error_msg, '\n')
    return(NULL)
  } else {
    num_posts <- wall$response$count
    if (num_posts == 0) {
      cat('The user has no posts\n')
      return(NULL)
    } else {
      offsets <- 100 * 0:floor(wall$response$count/100)
      howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('There are ', num_posts,' posts. Type the number of recent posts you want to consider (<= ', num_posts,'): '))))
      howmany <- ifelse(howmany <= num_posts, howmany, num_posts)
      while (is.na(howmany)) {
        howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('Wrong input! Try again! Type the number of recent posts you want to consider (<= ', num_posts,'): '))))
      }
      #--- Iteratively get reposts for every requested post
      total_output <- list()
      st <- proc.time()
      cat('Iterations started.\n')
      for (j in 1:howmany) {
        fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.getReposts?owner_id=', -group_id,'&post_id=', wall$response$items$id[j],'&need_likes=1&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
        if ('error' %in% names(fetched)) {
          cat('ERROR: ', fetched$error$error_msg, '\n')
          return(NULL)
        } else {
          items <- fetched$response$items
          if (!is.null(nrow(items))) {
            total_output[[j]] <- data.frame('repost_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'id']), NA, items[k, 'id'])),
                                            'reposter_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'from_id']), NA, items[k, 'from_id'])),
                                            'receiver_id' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'to_id']), NA, items[k, 'to_id'])),
                                            'date' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'date']), NA, items[k, 'date'])),
                                            'text' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'text']), NA, items[k, 'text'])),
                                            'num_comments' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'comments']['count']), NA, as.numeric(items[k, 'comments']['count']))),
                                            'num_likes' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'likes']['count']), NA, as.numeric(items[k, 'likes']['count']))), 
                                            'num_reposts' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'reposts']['count']), NA, as.numeric(items[k, 'reposts']['count']))), 
                                            'num_views' = sapply(1:nrow(items), function(k) ifelse(is.null(items[k, 'views']['count']), NA, as.numeric(items[k, 'views']['count'])))
                                            , stringsAsFactors = F)
            total_output[[j]]$reposted_user_id <- group_id
            total_output[[j]]$repost_of_post_id <- wall$response$items$id[j]
            if (verbose) {
              cat('post', j, '(out of', howmany, ') done\n')
            }
            Sys.sleep(1)
          }
        } # end: else
      } # end: for (j in 1:howmany)
      fin <- proc.time()
      cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
      total_output <- do.call('rbind', total_output)
      return(total_output)
    } # end: else below return(NULL)
  } # end: else below return(NULL) below ERROR
}


getGroupPostLikes <- function(group_id, post_id, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/likes.getList?type=post&owner_id=', -group_id,'&item_id=', post_id,'&count=1000&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
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
} 


getGroupWallLikes <- function(group_id, access_token, verbose = FALSE) {
  #--- Get posts number and request the number of posts to retrieve
  wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&fields=sex,bdate,city,country,timezone,photo_100,has_mobile,contacts,education,online,relation,last_seen,status,can_write_private_message,can_see_all_posts,can_post,universities&v=5.64&extended=0&access_token=', access_token))
  if ('error' %in% names(wall)) {
    cat('ERROR: ', wall$error$error_msg, '\n')
    return(NULL)
  } else {
    num_posts <- wall$response$count
    if (num_posts == 0) {
      cat('The user has no posts\n')
      return(NULL)
    } else {
      # offsets <- 100 * 0:floor(wall$response$count/100)
      howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('There are ', num_posts,' posts. Type the number of recent posts you want to consider (<= ', num_posts,'): '))))
      howmany <- ifelse(howmany <= num_posts, howmany, num_posts)
      while (is.na(howmany)) {
        howmany <- suppressWarnings(as.numeric(readline(prompt = paste0('Wrong input! Try again! Type the number of recent posts you want to consider (<= ', num_posts,'): '))))
      }
      #--- Get all requested posts
      cat('Retrieveing all requested posts\n')
      if (howmany <= 100) {
        all_requested_posts <- wall$response$items$id
      } else {
        all_requested_posts <- wall$response$items$id
        offsets <- 100 * 1:floor(howmany/100)
        for (w in 1:length(offsets)) {
          wall <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.get?owner_id=', -group_id,'&count=100&offset=', offsets[w],'&v=5.64&extended=0&access_token=', access_token))
          all_requested_posts <- c(all_requested_posts, wall$response$items$id)
        }
        all_requested_posts <- unique(all_requested_posts)
      }
      cat('All requested posts retrieved\n')
      #--- Iteratively get likes for every requested post
      total_output <- list()
      st <- proc.time()
      cat('Iterations started.\n')
      for (j in 1:howmany) {
          total_output[[j]] <- getGroupPostLikes(group_id = group_id, post_id = all_requested_posts[j], access_token = access_token)
          names(total_output)[j] <- all_requested_posts[j]
          if (verbose) {
            cat('post', j, '(out of', howmany, ') done\n')
          }
          Sys.sleep(1)
      }
      fin <- proc.time()
      cat('Total time:', as.numeric((fin-st)[3]/60), 'minutes\n')
      return(total_output)
    } # end: else below return(NULL)
  } # end: else below return(NULL) below ERROR
}


getGroupMostLikingUsers <- function(group_id, access_token, num_users = 'all', verbose = FALSE) {
  if (num_users != 'all') {
    while (is.na(as.numeric(num_users))) {
      num_users <- suppressWarnings(readline(prompt = paste0('Wrong input! Try again! How many most liking users to retrieve? Type all or an integer: ')))
      if (num_users != 'all') {
        num_users <- as.numeric(num_users)
      }
    }
  }
  likes <- getGroupWallLikes(group_id = group_id, access_token = access_token, verbose = verbose)
  likes <- do.call('c', likes)
  tb <- table(likes)
  output <- data.frame('user_id' = names(tb), 'num_likes' = as.numeric(tb), stringsAsFactors = F)
  output <- output[order(output$num_likes, decreasing = T),]
  if (num_users != 'all') {
    output <- output[1:num_users,]
  }
  return(output)
}


getGroupWallSearchCount <- function(group_id, query, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.search?owner_id=', -group_id,'&query=', query,'&count=2&v=5.64&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    return(fetched$response$count)
  }
}


searchGroupWall <- function(group_id, query, access_token) {
  fetched <- jsonlite::fromJSON(paste0('https://api.vk.com/method/wall.search?owner_id=', -group_id,'&query=', query,'&count=100&v=5.64&access_token=', access_token))
  if ('error' %in% names(fetched)) {
    cat('ERROR: ', fetched$error$error_msg, '\n')
    return(NULL)
  } else {
    if (length(fetched$response$items) > 0) {
      output <- data.frame('object_id' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$id[k]),
                           'from_user_id' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$from_id[k]),
                           'group_id_wall' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$owner_id[k]),
                           'date' = as.Date(as.POSIXct(fetched$response$items$date, origin="1970-01-01")),
                           'date_POSIXct' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$date[k]),
                           'post_type' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$post_type[k]),
                           'to_post_id' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$post_id[k]),
                           'text' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$text[k]),
                           'comments_count' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$comments$count[k]),
                           'likes_count' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$likes$count[k]),
                           'reposts_count' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$reposts$count[k]),
                           'views_count' = sapply(1:nrow(fetched$response$items), function(k) fetched$response$items$views$count[k]), stringsAsFactors = F)
      } else {
      output <- NULL
    }
    return(output)
  }
}


# Chance city and country codes into names: get all dataset, then covern city codes to city names for each country separately?

# Check: all functions return a data.frame with (if AAA %in% names(items)) check

# Add:
# wall.search: search post on a wall by a criterion

# check dates


