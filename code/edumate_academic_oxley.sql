	
	With average_mark as(
select task_id, 
decimal(avg(RAW_MARK),5,2) as average ,
decimal(stddev(RAW_MARK),5,2) as stddeviation
from 	stud_task_raw_mark 
where year(last_updated) = Year(current_date)
group by task_id),

Ranking as (
select task_id, student_id, raw_mark ,dense_rank() OVER (PARTITION BY task_id ORDER BY raw_mark DESC) as rank 
from 	stud_task_raw_mark 
where year(last_updated) = Year(current_date) and raw_mark is not null
),

Max_rank as ( 
select task_id, max(rank) as rank_out_of
from 	ranking
group by task_id
)



SELECT  distinct
	    form_run.form_run,
	    student.student_number,
	    contact.firstname as student_firstname,
	    contact.surname as student_surname,
	    course.course,
	    task.task,
	    coursework_task.due_date,
	    stud_task_raw_mark.raw_mark,
	    task.mark_out_of,
	    task.weighting,
	    am.average as task_average_mark,
	    am.stddeviation as standart_dev , 
	    case when am.stddeviation = 0 then 0
	    else decimal((stud_task_raw_mark.raw_mark - am.average )/ am.stddeviation,5,2)
	    end  as z_score,
	   	r.rank,
	    ma.rank_out_of
	   -- teacher_contact.FIRSTNAME as teacher_firstname,
	    --teacher_contact.surNAME as teacher_surname
			FROM table(edumate.get_enroled_students_form_run(current_date)) ges
			    INNER JOIN form_run ON form_run.form_run_id =ges.form_run_id
			    INNER JOIN student ON student.student_id = ges.student_id
			    INNER JOIN contact ON contact.contact_id = student.contact_id
			    INNER JOIN stud_task_raw_mark ON stud_task_raw_mark.student_id = student.student_id
			    join average_mark am on stud_task_raw_mark.TASK_ID  = am.TASK_ID
			   join ranking r on r.task_id = stud_task_raw_mark.TASK_ID and r.student_id = stud_task_raw_mark.STUDENT_ID
			    join max_rank ma on ma.task_id = stud_task_raw_mark.TASK_ID  
			    INNER JOIN task ON task.task_id = stud_task_raw_mark.task_id
			    Inner Join ACADEMIC_YEAR AY on AY.ACADEMIC_YEAR= year(Current_date)
			    INNER JOIN coursework_task ON coursework_task.task_id = task.task_id
			        AND coursework_task.academic_year_id = ay.academic_year_id
				--INNER JOIN classwork_task ON classwork_task.task_id = task.task_id
			        --AND classwork_task.academic_year_id = academic_year.academic_year_id
			    Inner join CLASS on class.course_id=coursework_task.course_id
			    --inner join class_teacher on class_teacher.CLASS_ID = class.CLASS_ID
			   -- inner join TEACHER on teacher.teacher_id = class_teacher.TEACHER_ID and class_teacher.IS_PRIMARY = 1
			    --inner join contact teacher_contact on teacher_contact.CONTACT_ID = teacher.CONTACT_ID
			    INNER JOIN course ON course.course_id = coursework_task.course_id
		
	
