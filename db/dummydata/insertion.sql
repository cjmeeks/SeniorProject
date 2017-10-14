insert into workout.users (first_name, last_name, username, weight, height, age) values ('CJ', 'Meeks', 'cjmeeks', 175, 160, 21);
insert into workout.users (first_name, last_name, username, weight, height, age) values ('test', 'asdf', 'test', 168, 163, 99);

insert into workout.lifts (lift_name) values ('lift1');
insert into workout.lifts (lift_name) values ('lift2');
insert into workout.lifts (lift_name) values ('lift3');
insert into workout.lifts (lift_name) values ('lift4');
insert into workout.lifts (lift_name) values ('lift5');
insert into workout.lifts (lift_name) values ('lift6');
insert into workout.lifts (lift_name) values ('lift7');
insert into workout.lifts (lift_name) values ('lift8');
insert into workout.lifts (lift_name) values ('lift9');


insert into workout.workouts (date, total_time, workout_type, user_id) values (now(), 600, 'testtype', 1);
insert into workout.workouts (date, total_time, workout_type, user_id) values (now(), 1200, 'testtype', 2);
insert into workout.workouts (date, total_time, workout_type, user_id) values (now(), 700, 'testtype', 1);
insert into workout.workouts (date, total_time, workout_type, user_id) values (now(), 7000, 'testtype', 2);

insert into workout.run (distance, time, mile_avg, speed_avg, workout_id) values (6, 3000, 8, 4, 1);
insert into workout.run (distance, time, mile_avg, speed_avg, workout_id) values (6, 2500, 8, 4, 2);

insert into workout.exercise (time, workout_id) values (600, 3);
insert into workout.exercise (time, workout_id) values (600, 4);

insert into workout.sets (weight, reps, exercise_id, lift_id) values (125, 12, 2, 1);
insert into workout.sets (weight, reps, exercise_id, lift_id) values (125, 12, 2, 1);
insert into workout.sets (weight, reps, exercise_id, lift_id) values (125, 12, 2, 1);
insert into workout.sets (weight, reps, exercise_id, lift_id) values (125, 12, 2, 1);

insert into workout.sets (weight, reps, exercise_id, lift_id) values (45, 12, 1, 2);
insert into workout.sets (weight, reps, exercise_id, lift_id) values (45, 12, 1, 2);
insert into workout.sets (weight, reps, exercise_id, lift_id) values (55, 12, 1, 2);




