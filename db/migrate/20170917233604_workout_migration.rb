class WorkoutMigration < ActiveRecord::Migration[5.1]
  def up
    execute <<-SQL
      CREATE TABLE workout.users (
        "user_id" SERIAL PRIMARY KEY,
        "first_name" TEXT NOT NULL,
        "last_name" TEXT NOT NULL,
        "username" TEXT NOT NULL UNIQUE,
        "password" TEXT NOT NULL,
        "weight" INT NOT NULL,
        "height" INT NOT NULL,
        "age" INT NOT NULL
      );
    SQL

      execute <<-SQL
        CREATE TABLE workout.workouts (
          "workout_id" SERIAL PRIMARY KEY,
          "date" Text NOT NULL,
          "total_time" DOUBLE PRECISION NOT NULL,
          "workout_name" TEXT NOT NULL,
          "user_id" INT NOT NULL references workout.users(user_id)
        );
      SQL
      
      execute <<-SQL
        CREATE TABLE workout.lifts (
          "lift_id" SERIAL PRIMARY KEY,
          "lift_name" VARCHAR NOT NULL
        );
      SQL

      execute <<-SQL
        CREATE TABLE workout.exercise (
          "exercise_id" SERIAL PRIMARY KEY,
          "time" INT NOT NULL,
          "workout_id" INT NOT NULL references workout.workouts(workout_id)
        );
      SQL

      execute <<-SQL
        CREATE TABLE workout.run (
          "run_id" SERIAL PRIMARY KEY,
          "distance" FLOAT NOT NULL,
          "time" DOUBLE PRECISION NOT NULL, 
          "mile_avg" FLOAT NOT NULL,
          "speed_avg" FLOAT NOT NULL,
          "workout_id" INT NOT NULL references workout.workouts(workout_id)
        );
      SQL

      execute <<-SQL
      CREATE TABLE workout.sets (
        "set_id" SERIAL PRIMARY KEY,
        "weight" INT NOT NULL,
        "reps" INT NOT NULL,
        "exercise_id" INT NOT NULL references workout.exercise(exercise_id),
        "lift_id" INT NOT NULL references workout.lifts(lift_id)
      );
    SQL

    execute <<-SQL
      CREATE TABLE workout.saved_workouts (
        "user_id" INT NOT NULL references workout.users(user_id), 
        "workout_id" INT NOT NULL references workout.workouts(workout_id)
      );
    SQL


  end

  def down
    execute <<-SQL
      DROP TABLE workout.users;
    SQL

    execute <<-SQL
      DROP TABLE workout.workouts;
    SQL

  end
end
