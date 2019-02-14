module User_id_type = 
 struct 
 type t = char
 let compare = Pervasives.compare
 let merge3 ~ancestor v1 v2 = '#'
 let resolve x y = '#'
end 

(* Tweet contains the tweet id, the person's name who tweeted and also the content of the tweet *)
module Tweet = 
 struct 
 type atom = User_id_type.t
 type t = {tweet_id : atom; author_name: string; content: string}
 let merge3 ~ancestor v1 v2 = {tweet_id = ancestor.tweet_id; author_name = ancestor.author_name; content = "#"}
 let resolve x y = {tweet_id = x.tweet_id; author_name = x.author_name; content = "#"}
end 

(* Timeline is the sequence of tweets which is represented here as list of tweets. But timeline 
   should always contain the tweets from the users that a particular user follow *)
module Timeline = 
 struct 
 type atom = User_id_type.t
 module OT = Mvector_list.List(Tweet)
 type t = {id : atom; tl : OT.t}
 let merge3 ~ancestor v1 v2 = {id = ancestor.id; tl = OT.merge3 (ancestor.tl) (v1.tl) (v2.tl)}
 let resolve x y = {id = x.id; tl = []}
end 

module User = 
 struct 
 module OI = User_id_type 
 module OL = Mvector_list.List(User_id_type)
 module OT = Mvector_list.List(Tweet)
 type atom = User_id_type.t
 type t = {id : atom; name: string; followers: OL.t; following: OL.t; user_home: OT.t}

 let merge3 ~ancestor v1 v2 = if (ancestor.id = v1.id) && (ancestor.id = v2.id) && (v1.id = v2.id) && 
                                 (ancestor.name = v1.name) && (ancestor.name = v1.name) && (v1.name = v2.name) then 
                                 {id = ancestor.id; name = ancestor.name; 
                                  followers = OL.merge3 (ancestor.followers) (v1.followers) (v2.followers); 
                                  following = OL.merge3 (ancestor.followers) (v1.followers) (v2.followers);
                                  user_home = OT.merge3 (ancestor.user_home) (v1.user_home) (v2.user_home)} else 
                                  failwith "Merge not possible" 

 let resolve x y = {id = x.id; name = x.name; followers = []; following = []; user_home = []}

 let get_name user = user.name

 let get_id user = user.id 

 let get_follower_of_user user = user.followers  

 let get_following_of_user user = user.following  

 let get_user_home user = user.user_home

 let rec get_index_follower follower_id user = match user.followers with 
                                           | [] -> failwith "No followers"
                                           | x :: xl -> if follower_id = x then 0 
                                             else (get_index_follower follower_id 
                                                  {id = user.id; 
                                                   name = user.name; 
                                                   followers = xl; 
                                                   following = user.following;
                                                   user_home = user.user_home}) + 1

 let add_follower_to_user follower_id user = {id = user.id; 
                                              name= user.name; 
                                              followers = List.append (user.followers) [follower_id]; 
                                              following = user.following;
                                              user_home = user.user_home}

 let remove_follower_from_user follower_id user = {id = user.id; 
                                                   name= user.name; 
                                                   followers = OL.delete user.followers 
                                                               (get_index_follower follower_id user); 
                                                   following = user.following;
                                                   user_home = user.user_home}

 let number_of_followers_of_user u = List.length(u.followers)

 let following_count_of_user u = List.length(u.following)

 let follow u_id user = {id = user.id; 
                         name= user.name; 
                         followers = user.followers; 
                         following = List.append (user.following) [u_id];
                         user_home = user.user_home}
end 

module Microblog = 
 struct 
 open Timeline
 open User
 module OU = Mvector_list.List(User)
 module OT = Mvector_list.List(Timeline)
 type t = {user_table : OU.t; timeline_table : OT.t}

 let get_user_table userTable = userTable.user_table

 let get_timeline_table userTable = userTable.timeline_table

 let rec search_user u_id userTable = match userTable.user_table with
                                  | [] -> failwith "No such user"
                                  | x :: x' -> if x.id = u_id then x 
                                    else search_user u_id {user_table = x'; 
                                                           timeline_table = userTable.timeline_table}

 let rec get_followers u_id userTable = match userTable.user_table with
                                  | [] -> failwith "No such user"
                                  | x :: x' -> if x.id = u_id then x.followers
                                    else get_followers u_id {user_table = x'; 
                                                             timeline_table = userTable.timeline_table}

 let rec add_follower u_id follower_id userTable = match userTable.user_table with 
                                   | [] -> failwith "No such user"
                                   | x :: x' -> if x.id = u_id then User.add_follower_to_user follower_id x 
                                     else add_follower u_id follower_id 
                                          {user_table = x';
                                           timeline_table= userTable.timeline_table}

 let rec remove_followers u_id follower_id userTable = match userTable.user_table with 
                                   | [] -> failwith "No such user"
                                   | x :: x' -> if x.id = u_id then User.remove_follower_from_user follower_id x 
                                     else remove_followers u_id follower_id 
                                          {user_table = x';
                                           timeline_table= userTable.timeline_table}

 let rec get_author_name u_id userTable = match userTable.user_table with 
                                      | [] -> failwith "No such user"
                                      | x :: x' -> if x.id = u_id then x.name else
                                        get_author_name u_id {user_table = x'; 
                                                              timeline_table = userTable.timeline_table}

 let add_tweet_user_home u_id tweet_author tweet_content user = List.append user.user_home 
                                                                              [{tweet_id = u_id; 
                                                                                author_name = tweet_author; 
                                                                                content = tweet_content}]

 let rec add_tweet_timeline u_id tweet_author tweet_content f_id userTable = match userTable.timeline_table with 
                                                                         | [] -> failwith "No such user"
                                                                         | x :: x' -> if x.id = f_id 
                                                                                      then {id = f_id;
                                                                                            tl = List.append x.tl 
                                                                                                             [{tweet_id = u_id; 
                                                                                                               author_name = tweet_author; 
                                                                                                               content = tweet_content}]}
                                                                                      else add_tweet_timeline u_id tweet_author tweet_content f_id 
                                                                                           {user_table = userTable.user_table;
                                                                                            timeline_table = x'} 

 let rec update_user_table u_id userTable userHome = match userTable.user_table with 
                                                | [] -> failwith "No such user"
                                                | x :: x' -> if x.id = u_id then 
                                                                            {id = u_id;
                                                                             name = x.name;
                                                                             followers = x.followers;
                                                                             following = x.following;
                                                                             user_home = userHome} :: x' 
                                                                            else
                                                                             x :: (update_user_table u_id 
                                                                             	                    {user_table = x';
                                                                                                     timeline_table = userTable.timeline_table}
                                                                                                     userHome)


 let do_new_tweet u_id tweet_author tweet_content userTable = 
 let fols = get_followers u_id userTable in 
 let u = search_user u_id userTable in 
 begin
 let up_user_home = add_tweet_user_home u_id tweet_author tweet_content u in 
 let up_user_table = update_user_table u_id userTable up_user_home in 
 let up_tl = List.map (fun f_id -> add_tweet_timeline u_id tweet_author tweet_content f_id userTable) fols in 
 {user_table = up_user_table; 
  timeline_table = up_tl}
 end 

 let merge ~ancestor v1 v2 = {user_table = OU.merge3 (ancestor.user_table) (v1.user_table) (v2.user_table);
                              timeline_table = OT.merge3 (ancestor.timeline_table) (v1.timeline_table) (v2.timeline_table)}



end


