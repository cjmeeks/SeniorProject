require "time"
require "json"

class InsertData < ActiveRecord::Migration[5.1]
  def change

    
    cj = ["cj", "meeks", "cjmeeks", 175, 160, 21]
    test = ["test", "test", "test", 175, 160, 21]
    

    
    lift1 = ["lift1"]
    lift2 = ["lift2"]
    lift3 = ["lift3"]
    lift4 = ["lift4"]
    

    w1 = [ Time.now, 3000, "run", 1]
    w2 = [ Time.now, 3000, "run", 1]
    w3 = [ Time.now, 3000, "run", 1]
    w4 = [ Time.now, 3000, "run", 1]
    

    run1 = [6, 3000, 8, 6, 1]
    run2 = [6, 3000, 8, 6, 2]

    e1 = [3000, 3]
    e2 = [3000, 3]

    s1 = [125, 12, 1,1]
    s2 = [125, 12, 1,1]
    s3 = [125, 12, 1,1]
    s4 = [34, 12, 1,1]
    s5 = [34, 12, 1,1]
    s6 = [34, 12, 1,1]

    userQuery = []
    userQuery << "insert into workout.users (first_name, last_name, username, weight, height, age) values"
    userQuery << "('#{cj[0]}','#{cj[1]}','#{cj[2]}',#{cj[3]},#{cj[4]},#{cj[5]})"

    liftQuery = []
    liftQuery << "insert into workout.lifts (lift_name) values"
    liftQuery << "('#{lift1[0]}'), "
    liftQuery << "('#{lift2[0]}'), "
    liftQuery << "('#{lift3[0]}'), "
    liftQuery << "('#{lift4[0]}')"

    workoutQuery = []
    workoutQuery << "insert into workout.workouts (date, total_time, workout_type, user_id) values "
    workoutQuery << "('#{w1[0]}',#{w1[1]},'#{w1[2]}',#{w1[3]}), "
    workoutQuery << "('#{w2[0]}',#{w2[1]},'#{w2[2]}',#{w2[3]}), "
    workoutQuery << "('#{w3[0]}',#{w3[1]},'#{w3[2]}',#{w3[3]}), "
    workoutQuery << "('#{w4[0]}',#{w4[1]},'#{w4[2]}',#{w4[3]}) "

    runQuery = []
    runQuery << "insert into workout.run (distance, time, mile_avg, speed_avg, workout_id) values "
    runQuery << "(#{run1[0]},#{run1[1]},#{run1[2]},#{run1[3]},#{run1[4]}), "
    runQuery << "(#{run2[0]},#{run2[1]},#{run2[2]},#{run2[3]},#{run2[4]})"

    exerciseQuery = []
    exerciseQuery << "insert into workout.exercise (time, workout_id) values "
    exerciseQuery << "(#{e1[0]},#{e1[1]}), "
    exerciseQuery << "(#{e2[0]},#{e2[1]})"

    setQuery = []
    setQuery << "insert into workout.sets (weight, reps, exercise_id, lift_id) values "
    setQuery << "(#{s1[0]},#{s1[1]},#{s1[2]},#{s1[3]}), "
    setQuery << "(#{s2[0]},#{s2[1]},#{s2[2]},#{s2[3]}), "
    setQuery << "(#{s3[0]},#{s3[1]},#{s3[2]},#{s3[3]}), "
    setQuery << "(#{s4[0]},#{s4[1]},#{s4[2]},#{s4[3]}), "
    setQuery << "(#{s5[0]},#{s5[1]},#{s5[2]},#{s5[3]}), "
    setQuery << "(#{s6[0]},#{s6[1]},#{s6[2]},#{s6[3]})"

    execute userQuery.join(" ")
    execute liftQuery.join(" ")
    execute workoutQuery.join(" ")
    execute runQuery.join(" ")
    execute exerciseQuery.join(" ")
    execute setQuery.join(" ")

  end
end
