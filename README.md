# Twitter_Stock_Client
## Setup
  Make sure the following requirements are installed ```brew install gmp``` and ```brew install pkg-config``` \
  Go to project directory and run ```opam install . --deps-only``` which runs the ```requirements.opam``` file to install the necessary dependencies. 
## Description
  This program scrapes twitter handles for stock pumps. It then takes the stock and purchases it with a high-frequency trading strategy.
## Usage
  ```make start``` to build files and run main.ml\
  ```make test``` to run OUnit test suite
