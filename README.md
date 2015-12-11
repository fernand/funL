The premise is that it is really hard with existing tools (SQL-like languages or Mixpanel) to make ad-hoc funnel-type queries on large streams of events which are user (or object) centric. funL runs on top of any Hadoop Streaming cluster.

Say that you have a lot of JSON event data (on S3 or HDFS) in this form:
```
{"event": "view", "properties":{source:"social", "time":1329263748, "distinct_id":22}}
{"event": "signup", "properties":{source:"social", "time":1329263888, "distinct_id":22}}
{"event": "view", "properties":{source:"email", "time":1329277748, "distinct_id":22}}
{"event": "click", "properties":{source:"email", "time":1329277794, "distinct_id":22}}
```

Where `distinct_id` is a user or some kind of object sending events.

You want to know the number of users who joined in Q1 2014, and in who in Q2 returned to view a page from an email you sent and then also clicked on something. Doing this in SQL would require a bunch of big joins (if you have a table per event) and some date manipulation. It's definitely doable but becomes increasingly complicated as you add more events in your funnel.

In funL, the query would look like
```
1 quarter [signup()];
1 quarter [view(source='email'), click()] ("2014","2015")
```

The date span [2014,2015[ is specified on the second line so funL figures out that you want to bind the second part of the query to only in quarters in 2014, so it figures out that the first line is the quarter preceding any of these respective 2014 quarters.

This is what using funL looks like:

1. You type a query on a local console
2. The query is serialized and written to S3
3. A new Streaming Map job is sent to a Hadoop cluster (these cluster have a local binary of the Haskell module which is then run in Hadoop Streaming mode). The first Map job grabs the relevant events (you don't have to look for events outside a date range), groups them by user, and orders them chronologically
4. A second Map job runs your query for a user by looking whether they fall in the funnel you defined
5. The output is then fed in a Streaming Reduce job to count the number of users by date range
6. The query results end up on S3 and are pushed to your console.

The Haskell module does 2, 3, 4, and 5.
I have not included the glue code which makes it work on EMR (generating MapReduce jobs, copying files to nodes, returning the query result, local query console) because there’s too much sensitive company information.

So the code you’re looking at includes the query parsing logic and the translation logic from a query to actual streaming Map or Reduce job.

A funL query is essentially a predicate test against a series of ordered events per user. For each user's time series we test whether they fall in the funnel for a specific date range.

funL supports multiple ways of combining events: you can use `and`, `or`, or `seq` on an array of events depending on what you want to do. It also supports more advanced queries, but I won't go into detail here.
