-record(static, {
    rate = 1 :: drip:rand_range(),
    rng = undefined :: undefined | drip:rand_fun()
}).

-record(time_based, {
    desired_per_second = 1 :: drip:rand_range(),
    sample_rate = 1 :: drip:rand_range(),
    rng = undefined :: undefined | drip:rand_fun(),
    minimum = 0 :: non_neg_integer(),
    ms_period = 15000 :: pos_integer()
}).
