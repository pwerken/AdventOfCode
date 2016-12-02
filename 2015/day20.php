<?php

$arr = [];
for($i = 1; $i < 1000000; $i++)
{
    for($j = 1; $i*$j < 1000000; $j ++) {
        if(!isset($arr[$i * $j]))
            $arr[$i*$j] = $i * 10;
        else
            $arr[$i*$j] += $i * 10;
    }

    if($arr[$i] > 29000000) {
        echo $i . "\n";
        break;
    }
}

$arr = [];
for($i = 1; $i < 1000000; $i++)
{
    for($j = 1; $j <= 50 && $i*$j < 1000000; $j ++) {
        if(!isset($arr[$i * $j]))
            $arr[$i*$j] = $i * 11;
        else
            $arr[$i*$j] += $i * 11;
    }

    if($arr[$i] > 29000000) {
        echo $i . "\n";
        break;
    }
}
