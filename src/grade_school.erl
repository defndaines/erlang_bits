-module(grade_school).
-export([new/0, add/3, get/2, sort/1]).

new() ->
  [].

add(Student, Grade, GradeBook) ->
  [{Grade, Student} | GradeBook].

get(FindGrade, GradeBook) ->
  [Student || {Grade, Student} <- GradeBook, Grade =:= FindGrade].

sort(GradeBook) ->
  Map = lists:foldl(fun reducer/2, maps:new(), GradeBook),
  maps:to_list(Map).

reducer({Grade, Student}, GradeBook) ->
  case maps:find(Grade, GradeBook) of
    {ok, Students} -> maps:update(Grade, lists:sort([Student | Students]), GradeBook);
    error -> maps:put(Grade, [Student], GradeBook)
  end.