gen stateabbrev = subinstr(statename, "Alabama", "AL", 1)
replace stateabbrev = subinstr(stateabbrev, "Alaska", "AK", 1)
replace stateabbrev = subinstr(stateabbrev, "Arizona", "AZ", 1)
replace stateabbrev = subinstr(stateabbrev, "Arkansas", "AR", 1)
replace stateabbrev = subinstr(stateabbrev, "California", "CA", 1)
replace stateabbrev = subinstr(stateabbrev, "Colorado", "CO", 1)
replace stateabbrev = subinstr(stateabbrev, "Connecticut", "CT", 1)
replace stateabbrev = subinstr(stateabbrev, "Delaware", "DE", 1)
replace stateabbrev = subinstr(stateabbrev, "District of Columbia", "DC", 1)
replace stateabbrev = subinstr(stateabbrev, "Florida", "FL", 1)
replace stateabbrev = subinstr(stateabbrev, "Georgia", "GA", 1)
replace stateabbrev = subinstr(stateabbrev, "Hawaii", "HI", 1)
replace stateabbrev = subinstr(stateabbrev, "Idaho", "ID", 1)
replace stateabbrev = subinstr(stateabbrev, "Illinois", "IL", 1)
replace stateabbrev = subinstr(stateabbrev, "Indiana", "IN", 1)
replace stateabbrev = subinstr(stateabbrev, "Iowa", "IA", 1)
replace stateabbrev = subinstr(stateabbrev, "Kansas", "KS", 1)
replace stateabbrev = subinstr(stateabbrev, "Kentucky", "KY", 1)
replace stateabbrev = subinstr(stateabbrev, "Louisiana", "LA", 1)
replace stateabbrev = subinstr(stateabbrev, "Maine", "ME", 1)
replace stateabbrev = subinstr(stateabbrev, "Maryland", "MD", 1)
replace stateabbrev = subinstr(stateabbrev, "Massachusetts", "MA", 1)
replace stateabbrev = subinstr(stateabbrev, "Michigan", "MI", 1)
replace stateabbrev = subinstr(stateabbrev, "Minnesota", "MN", 1)
replace stateabbrev = subinstr(stateabbrev, "Mississippi", "MS", 1)
replace stateabbrev = subinstr(stateabbrev, "Missouri", "MO", 1)
replace stateabbrev = subinstr(stateabbrev, "Montana", "MT", 1)
replace stateabbrev = subinstr(stateabbrev, "Nebraska", "NE", 1)
replace stateabbrev = subinstr(stateabbrev, "Nevada", "NV", 1)
replace stateabbrev = subinstr(stateabbrev, "New Hampshire", "NH", 1)
replace stateabbrev = subinstr(stateabbrev, "New Jersey", "NJ", 1)
replace stateabbrev = subinstr(stateabbrev, "New Mexico", "NM", 1)
replace stateabbrev = subinstr(stateabbrev, "New York", "NY", 1)
replace stateabbrev = subinstr(stateabbrev, "North Carolina", "NC", 1)
replace stateabbrev = subinstr(stateabbrev, "North Dakota", "ND", 1)
replace stateabbrev = subinstr(stateabbrev, "Ohio", "OH", 1)
replace stateabbrev = subinstr(stateabbrev, "Oklahoma", "OK", 1)

replace stateabbrev = "OR" if statename == "Oregon"
replace stateabbrev = "PA" if statename == "Pennsylvania"
replace stateabbrev = "RI" if statename == "Rhode Island"
replace stateabbrev = "SC" if statename == "South Carolina"
replace stateabbrev = "SD" if statename == "South Dakota"
replace stateabbrev = "TN" if statename == "Tennessee"
replace stateabbrev = "TX" if statename == "Texas"
replace stateabbrev = "UT" if statename == "Utah"
replace stateabbrev = "VT" if statename == "Vermont"
replace stateabbrev = "VA" if statename == "Virginia"
replace stateabbrev = "WA" if statename == "Washington"
replace stateabbrev = "WV" if statename == "West Virginia"
replace stateabbrev = "WI" if statename == "Wisconsin"
replace stateabbrev = "WY" if statename == "Wyoming"
