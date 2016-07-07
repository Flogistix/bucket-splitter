# Bucket Splitter

This project will gather all objects that are stored in a S3 bucket 
and divide them into the number of specified groups. The resulting groups 
of keys will be dumped into the current directory as file.

This project requires your AWS credentials be stored in the environment variables.


    bucket-splitter
      -b BUCKETNAME  --bucket=BUCKETNAME  Bucket name that contains the objects to be divided.
      -r us-east-1   --region=us-east-1   Set region to operate in. S3 requires you specifiy what region the bucket is in.
      -d NUM         --divideBy=NUM       Divide the collection of objects N times.
      -h             --help               Show help.

