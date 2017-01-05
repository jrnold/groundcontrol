# groundcontrol

Roll your own on-time flight dataset, like [nycflights13](https://github.com/hadley/nycflights13) using the airports and years of your choosing.

For example, this will create a new data-only R package named **seaflights15**
with all flights departing or arriving at SEA (Seattle-Tacoma airport) in 2015.
```
groundcontrol::create_flights("seaflights15", "SEA", 2015,
                              origin = TRUE, dest = TRUE)
```

