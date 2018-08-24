WITH student_classes AS
    (
    SELECT
        view_student_class_enrolment.student_id,
        class.class AS CLASS_NAME,
        course.code||'_'||class.identifier AS CLASS_CODE,
        class.class_id,class.class_type_id,
        REPLACE(salutation.salutation,'Unspecified','') AS TEACHER_TITLE,
        contact.firstname AS TEACHER_FIRSTNAME,
        contact.surname AS TEACHER_SURNAME,contact.EMAIL_ADDRESS  AS TEACHER_EMAIL,
        ROW_NUMBER() OVER (PARTITION BY view_student_class_enrolment.student_id ORDER BY view_student_class_enrolment.end_date DESC, view_student_class_enrolment.start_date DESC) AS CLASS_NUM
    FROM view_student_class_enrolment
        INNER JOIN class_teacher ON class_teacher.class_id = view_student_class_enrolment.class_id
            AND class_teacher.is_primary = 1
        INNER JOIN teacher ON teacher.teacher_id = class_teacher.teacher_id
        INNER JOIN contact ON contact.contact_id = teacher.contact_id
        LEFT JOIN salutation ON salutation.salutation_id = contact.salutation_id
        LEFT JOIN class ON class.class_id = view_student_class_enrolment.class_id
        LEFT JOIN course ON course.course_id = view_student_class_enrolment.course_id
    WHERE DATE(current_date) BETWEEN view_student_class_enrolment.start_date AND view_student_class_enrolment.end_date
--        AND class.class_type_id NOT IN (6,8,10,13,14,1000,1001,1002,1003)
        AND (
                class.class_type_id NOT IN (6,8,10,13,14,1000,1001,1002,1003)
                OR 
                upper(class.class) like '%STUDY%'
--                upper(class.class) IN ('11 STUDY FRIB6','11IB STUDY FRIB5')                                      
            )
    ),
    
    student_tutor_class AS (
    SELECT
        view_student_class_enrolment.student_id,
        class.class AS CLASS_NAME,
        course.code||'_'||class.identifier AS CLASS_CODE,
        class.class_id,class.class_type_id,
        REPLACE(salutation.salutation,'Unspecified','') AS TEACHER_TITLE,
        contact.firstname AS TEACHER_FIRSTNAME,
        contact.surname AS TEACHER_SURNAME,contact.EMAIL_ADDRESS  AS TEACHER_EMAIL,
        ROW_NUMBER() OVER (PARTITION BY view_student_class_enrolment.student_id ORDER BY view_student_class_enrolment.end_date DESC, view_student_class_enrolment.start_date DESC) AS CLASS_NUM
    FROM view_student_class_enrolment
        INNER JOIN class_teacher ON class_teacher.class_id = view_student_class_enrolment.class_id
            AND class_teacher.is_primary = 1
        INNER JOIN teacher ON teacher.teacher_id = class_teacher.teacher_id
        INNER JOIN contact ON contact.contact_id = teacher.contact_id
        LEFT JOIN salutation ON salutation.salutation_id = contact.salutation_id
        LEFT JOIN class ON class.class_id = view_student_class_enrolment.class_id
        LEFT JOIN course ON course.course_id = view_student_class_enrolment.course_id
    WHERE DATE(current_date) BETWEEN view_student_class_enrolment.start_date AND view_student_class_enrolment.end_date
      and class.class_type_id = 2
    )
    
SELECT distinct
     -- student.reg_number as STUDENT_ID,
     student_number as STUDENT_ID,
     gender.GENDER,
     left(gender.GENDER,1) as GENDER_ID,
     firstname as STUDENT_FIRSTNAME,
     surname as STUDENT_SURNAME,
     contact.email_address AS STUDENT_EMAIL,
     ltrim(replace(replace(form_run, year(current_date),''),'Year','')) as YEAR,
     right(student_tutor_class.class_code,2) as TUTOR_GROUP,
     form_run as COHORT,
     course.COURSE as COURSE,
     class as CLASS,
     course.code || ' ' || class.identifier as CLASS_CODE,
--     student_classes.class_type_id,
     student_classes.teacher_title AS TEACHER_TITLE,
     student_classes.teacher_firstname AS TEACHER_FIRSTNAME,
     student_classes.teacher_surname AS TEACHER_SURNAME,
     student_classes.teacher_email AS TEACHER_EMAIL
--     course.code || ' ' || class.identifier as "class code"
--     class_enrollment.start_date,
--     class_enrollment.end_date
FROM
    table(edumate.get_enroled_students_form_run(  current_date )) gesfr
    INNER JOIN student on gesfr.student_id = student.student_id
    INNER JOIN contact on student.contact_id = contact.contact_id
    INNER JOIN edumate.GENDER gender on contact.GENDER_ID = gender.GENDER_ID
    INNER JOIN form_run on gesfr.form_run_id = form_run.form_run_id
    Inner Join form on form_run.FORM_ID = form.FORM_ID
    INNER JOIN student_classes ON student_classes.student_id = student.student_id
    INNER JOIN student_tutor_class ON student_tutor_class.student_id = student.student_id
    inner JOIN class_enrollment on gesfr.student_id = class_enrollment.student_id
    INNER JOIN class on student_classes.class_id = class.class_id
    INNER JOIN academic_year on academic_year.academic_year_id = class.academic_year_id
    INNER JOIN course on course.course_id = class.course_id
WHERE
    current_date  between class_enrollment.start_date and class_enrollment.end_date
    and form.form_id IN (9,10,11,12,13,14,15,16)
    -- Ignore the following class types
    --    ** Extra Subjects (XS)
    --    ** CoCurricular Class (CC)
    --    ** Senior School Sport (SSP)
    --    ** Winter Sport (WSP)
    --    ** Cross Country (XC)
    --    ** IB Community Action Service (BCAS)
    and  NOT (upper(class.class) LIKE 'XS%' OR upper(class.class) LIKE 'CC%' OR upper(class.class) LIKE 'SSP%' 
             OR upper(class.class) LIKE 'WSP%' OR upper(class.class) LIKE 'XC%' OR upper(course.code) LIKE '1_BCAS%'
            )
    and NOT (upper(class.class) LIKE '%MUSIC%COMPOSITION%' OR upper(class.class) LIKE '%MUSIC%LISTENING%' OR upper(class.class) LIKE '%MUSIC%PERFORM%')
    and NOT (upper(class.class) LIKE '%IB%CORE%')
    and NOT (upper(class.class) LIKE 'IB%EE%' OR upper(class.class) LIKE '%ICAS%' OR upper(class.class) LIKE 'DOE%' or  upper(class.class) LIKE '%DUKE%')
    and NOT (upper(class.class) LIKE '%DEBATING%' OR upper(class.class) LIKE '%BAND%')  
    and NOT (upper(class.class) LIKE '%ENSEMBLE%' OR upper(class.class) LIKE '%DUX%')
    and NOT (upper(class.class) LIKE '%STUDY%' OR upper(class.class) LIKE '%FENCING%')
    and NOT (upper(class.class) LIKE '%KAYAK%' OR upper(class.class) LIKE '%BIKING%')
    and NOT (upper(class.class) LIKE '%VOX%' OR upper(class.class) LIKE '%CHORALE%')
    and NOT (upper(class.class) LIKE '%EXTRA%')  -- for extra PE classes, etc
    and NOT (upper(class.class) LIKE '%TUTOR%' OR upper(class.class) LIKE '%BASKETBALL%')
    and NOT (upper(class.class) LIKE '%POLO%' OR upper(class.class) LIKE '%ASSEMBLY%')
    and NOT (upper(class.class) LIKE '%DISTANCE%' OR upper(class.class) LIKE '%WIDE%')
    and NOT (upper(class.class) LIKE '%PRODUCTION%')
    and NOT (upper(class.class) LIKE '%ISA %')
    and NOT (upper(class.class) LIKE '%FILM%')
    and NOT (upper(class.class) LIKE '%SENIOR%') -- for Senior Science course

    --and (ltrim(replace(replace(form_run, year(current_date),''),'Year','')) LIKE '12%'
      --  or ltrim(replace(replace(form_run, year(current_date),''),'Year','')) LIKE '11%'
      --  or ltrim(replace(replace(form_run, year(current_date),''),'Year','')) LIKE '10%'
      --  or ltrim(replace(replace(form_run, year(current_date),''),'Year','')) LIKE '9%'
      --  or ltrim(replace(replace(form_run, year(current_date),''),'Year','')) LIKE '8%'
      --  or ltrim(replace(replace(form_run, year(current_date),''),'Year','')) LIKE '7%')
    and student_classes.class_code = (course.code || '_' || class.identifier)
    and (   
            class.CLASS_TYPE_ID IN (1,2)
            or 
            ((class.COURSE_ID, form_run.FORM_RUN_ID)  not in 
                   (select rpc.COURSE_ID, rpf.FORM_RUN_ID from report_period rp 
                    join REPORT_PERIOD_COURSE rpc on rp.REPORT_PERIOD_ID = rpc.REPORT_PERIOD_ID
                    join REPORT_PERIOD_FORM_RUN rpf on rpf.REPORT_PERIOD_ID =rp.REPORT_PERIOD_ID
                    join ACADEMIC_YEAR ay on ay.ACADEMIC_YEAR_ID = rp.ACADEMIC_YEAR_ID
                    where year(  current_date ) =ay.ACADEMIC_YEAR                    
                   )
            )
            or upper(class.class) like '%xSTUDY%'
                        or upper(class.class) like '%LEARNING PLUS%'
        )
--  and upper(class.class) like '%STUDY%'
----and surname = 'Baker'
ORDER BY    student_id, cohort, class