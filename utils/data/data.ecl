% lecture(Id, Duration, Type, Professors, Groups).
lecture(1, 1, "Lab", [1],    [1, 3]).

lecture(2, 2, "Aud", [2],    [1]).
lecture(3, 2, "Lab", [1, 2], [2]).

lecture(4, 3, "Aud", [3],    [1, 2, 3]).
lecture(5, 3, "Aud", [4],    [3]).
lecture(6, 3, "Lab", [1, 4], [2, 3]).

lecture(7, 1, "Lab", [1, 2],     [1, 2, 3]).
lecture(8, 2, "Lab", [2, 4],     [12, 11, 10]).
lecture(9, 3, "Lab", [1, 2],     [12, 5, 3]).
lecture(10, 1, "Aud", [3, 1],    [11, 5]).
lecture(11, 2, "Aud", [1, 2, 4], [2, 5, 7]).
lecture(12, 3, "Aud", [1, 4],    [1, 2]).

% group(Id, MemberCount, Overlapping).
group(1,  90, [2, 3]).
group(2,  71, [1, 3]).
group(3,  47, [1, 2]).
group(4,  62, [5, 6]).
group(5,  95, [4, 6]).
group(6,  91, [4, 5]).
group(7,  19, [8, 9]).
group(8,  47, [7, 9]).
group(9,  86, [7, 8]).
group(10, 48, [11, 12]).
group(11, 46, [10, 12]).
group(12, 16, [10, 11]).
 
% professor(Id, Times).
professor(1, [3, 4, 5, 7, 11, 13, 16, 18, 20, 22, 24, 25, 26, 31, 33, 37, 38, 39, 41, 44, 45, 46, 52, 54, 56, 57, 63, 64, 67, 68]).
professor(2, [1, 2, 5, 6, 7, 8, 10, 13, 14, 17, 18, 20, 21, 23, 25, 26, 27, 28, 29, 30, 33, 34, 36, 38, 39, 40, 43, 44, 45, 47, 48, 50, 51, 52, 53, 54, 57, 61, 62, 66, 67]).
professor(3, [1, 6, 8, 11, 12, 13, 15, 16, 17, 20, 21, 23, 24, 25, 26, 28, 32, 33, 35, 36, 37, 39, 41, 45, 46, 48, 52, 55, 56, 63, 66, 68]).
professor(4, [4, 8, 9, 13, 15, 18, 26, 29, 30, 31, 32, 33, 35, 36, 38, 43, 44, 45, 50, 51, 54, 58, 59, 63, 66, 68]).

% room(Id, Type, Capacity, Times).
room(1,  "Aud", 500, [1, 2, 3, 6, 8, 10, 11, 13, 14, 15, 16, 17, 19, 20, 21, 22, 25, 26, 30, 32, 33, 34, 36, 38, 43, 45, 47, 49, 51, 52, 53, 54, 56, 57, 58, 59, 61, 62, 65, 66]).
room(2,  "Aud", 500, [1, 5, 8, 12, 13, 19, 20, 21, 23, 31, 33, 35, 38, 39, 40, 41, 42, 43, 45, 46, 50, 51, 54, 55, 57, 59, 60, 61, 62, 63, 65, 66, 68]).
room(3,  "Lab", 500, [1, 2, 3, 5, 6, 9, 10, 11, 12, 13, 15, 16, 18, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 36, 37, 39, 40, 41, 44, 47, 48, 49, 50, 51, 52, 53, 54, 58, 59, 60, 61, 63, 66, 67, 68]).
room(4,  "Aud", 500, [1, 2, 3, 4, 5, 7, 8, 10, 12, 13, 14, 15, 16, 18, 19, 25, 27, 28, 29, 30, 31, 32, 34, 35, 36, 37, 38, 39, 41, 42, 44, 45, 46, 47, 49, 50, 51, 53, 55, 57, 59, 60, 61, 65]).
room(5,  "Lab", 500, [3, 5, 6, 7, 8, 11, 12, 13, 14, 15, 17, 19, 20, 22, 23, 26, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 39, 40, 42, 44, 45, 46, 47, 49, 51, 52, 54, 56, 57, 60, 62, 63, 64, 65, 66, 67, 68]).
room(6,  "Lab", 500, [1, 2, 6, 8, 10, 13, 14, 15, 25, 26, 28, 30, 31, 32, 34, 37, 41, 44, 45, 47, 49, 50, 51, 54, 56, 57, 59, 62, 63, 64, 67]).
room(7,  "Lab", 500, [1, 2, 3, 4, 5, 6, 8, 9, 10, 14, 17, 20, 21, 22, 26, 27, 28, 29, 35, 36, 37, 38, 39, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 55, 57, 58, 59, 61, 62, 63, 65, 66]).
room(8,  "Lab", 500, [9, 14, 19, 20, 21, 24, 26, 32, 36, 37, 39, 40, 43, 48, 51, 53, 54, 56, 59, 61, 65, 67]).
room(9,  "Lab", 500, [2, 3, 5, 13, 16, 17, 19, 22, 23, 28, 32, 40, 41, 42, 47, 50, 52, 53, 55, 56, 57, 58, 59, 60]).
room(10, "Aud", 500, [1, 3, 5, 7, 8, 10, 11, 14, 15, 16, 17, 18, 20, 22, 29, 30, 32, 34, 39, 40, 46, 51, 53, 60, 67]).
