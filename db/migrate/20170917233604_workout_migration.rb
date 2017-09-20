class WorkoutMigration < ActiveRecord::Migration[5.1]
  def up
    execute <<-SQL
      CREATE TABLE workout.users (
        "user_id" SERIAL PRIMARY KEY,
        "first_name" TEXT NOT NULL,
        "last_name" TEXT NOT NULL,
        "username" TEXT NOT NULL,
        "weight" FLOAT NOT NULL,
        "height" FLOAT NOT NULL,
        "age" INT NOT NULL
      );
      SQL

      execute <<-SQL
        CREATE TABLE workout.workouts (
          "saved_search_id" SERIAL PRIMARY KEY,
          "date" TIMESTAMPTZ NOT NULL,
          "total_time" TIME NOT NULL,
          "workout_type" TEXT NOT NULL,
          "user_id" INTEGER NOT NULL references workout.users(user_id)
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
