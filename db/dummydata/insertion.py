import psycopg2

database = "senior_project"
host = "localhost"
port = 5432
username = "cjmeeks"
password = "cjmeeksdb"
fd = open('user.sql', 'r')
users = fd.read()
fd.close()
users = users.split(';')

def connect():
  conn = None
  try:
    print('Connecting to the PostgreSQL database...')
    conn = psycopg2.connect("dbname=senior_project user=cjmeeks password=cjmeeksdb host=localhost")
    cur = conn.cursor()
    print('PostgreSQL database version:')
    # for user in users:
    #   try:
    #     cur.execute(user)
    #   except(Exception, psycopg2.DatabaseError) as msg:
    #     print("Command skipped: ", msg)
    cur.execute('insert into workout.users (first_name, last_name, username, weight, height, age) values ("cj", "meeks", "cjmeeks", 175, 160, 21)')
    cur.close()

  except (Exception, psycopg2.DatabaseError) as error:
      print(error)
  finally:
      if conn is not None:
          conn.close()
          print('Database connection closed.')

connect()
print("hello im good")
