class CreateWorkoutMigration < ActiveRecord::Migration[5.1]
  def up
    execute <<-SQL
      CREATE SCHEMA "workout";
    SQL
  end

  def down
    execute <<-SQL
      DROP SCHEMA "workout";
    SQL
  end
end
