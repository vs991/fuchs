#!/usr/bin/env ruby

# Vision API
# Analyze IG post pictures using Google Vision API for ML purposes
# Dr.V.
# VERSION : 0.2
#


require "google/cloud/vision"
require "csv"
require_relative '../db'


def thevisionthing(postid)

  sql = "select case when images -> 'standard_resolution' -> 'url' is null then images -> 'thumbnail_src' else images -> 'standard_resolution' -> 'url' end as url from \"HistoryItem\" where \"id\" = '#{postid}';"
  puts postid

  if ENV['where'] == 'local'
    res = connectDb.exec(sql)
  else
    res = connectDbserver.exec(sql)
  end
  if res[0]['url'].nil? == false
    # puts res[0]['url']
    url = res[0]['url'].gsub(/\\|"/,'')



    vision = Google::Cloud::Vision.new

    image  = vision.image url #"otter_crossing.jpg"
    labels={}

    begin
      image.labels.each do |label|
        if label.description.size > 0
          labels[label.description.downcase]=label.score
        end
      end

      web = image.web

      entities={}
      web.entities.each do |entity|
        if entity.description.size > 0
          entities[entity.description.downcase]=1
        end
      end

      # entities = entities.merge(labels)

      # colors={}
      # rgb={}
      #
      # image.properties.colors.each_with_index do |color,index|
      #   colors[index.to_s]=color.to_s[/(?<=\(colors: ).*(?=\))/]
      #   rgb[index.to_s]=[color.red,color.green,color.blue]
      #   index += 1
      # end
    rescue
       puts 'Image not valid.'
       entities={}
       colors={}
       rgb=[]
    end

  else
    puts res[0]
    puts 'URL not valid.'
    entities={}
    colors={}
    rgb=[]

  end

  return entities,labels,colors,rgb
end

# postid='0875c175-f64f-4129-871a-2f0ba72a03a0'
# postid='fd989848-3d9d-475e-a345-f089942bee18'
# postid='ae732749-0596-4984-89c7-9013399af4c3'
# puts thevisionthing(postid)
# abort
