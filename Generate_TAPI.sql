CREATE OR REPLACE PACKAGE generate_pkg AS

 

   PROCEDURE tapi( p_table_name   IN    VARCHAR2,
                   p_view_name    IN    VARCHAR2 DEFAULT NULL );

 

   PROCEDURE create_view( p_view_name VARCHAR2 );

 

END generate_pkg;

/

 

 

 

CREATE OR REPLACE PACKAGE BODY generate_pkg AS

 

   PROCEDURE debug( p_string IN VARCHAR2 )

   IS

      l_string LONG DEFAULT p_string;

   BEGIN

      LOOP

        EXIT WHEN l_string IS NULL;

        DBMS_OUTPUT.PUT_LINE( SUBSTR( l_string, 1, 250 ) );

        l_string := SUBSTR( l_string, 251 );

      END LOOP;

   END;

 

 

   PROCEDURE tapi( p_table_name   IN    VARCHAR2,
                   p_view_name    IN    VARCHAR2 DEFAULT NULL ) IS

 

   l_final_sql    CLOB;
   l_spacer       INT := 32;
   l_tabstops     INT := 3;
   l_package_name VARCHAR2(30) := SUBSTR('TAPI_'||UPPER(p_table_name),1,30);
   l_object_name  VARCHAR2(30);

 

   BEGIN

 

      DELETE
      FROM tapi_gen
      WHERE table_name = p_table_name;

 

      IF p_view_name IS NOT NULL THEN

         l_object_name := p_view_name;

      ELSE

         l_object_name := p_table_name;

      END IF;

 

      l_final_sql := 'CREATE OR REPLACE PACKAGE '||LOWER(l_package_name)||' AS'||CHR(13)||CHR(13);

 

   /* --------------------------------------------------------------------------------------

      Retrieve the LINES for the INSERT PROCEDURE for PACKAGE HEADER

      -------------------------------------------------------------------------------------- */

 

      FOR i IN ( SELECT DECODE( lag(column_name) over  ( ORDER BY column_id),NULL, '   PROCEDURE insert_row( ','                         ') ||'p_'||RPAD(LOWER( column_name ),8 * l_tabstops,CHR(l_spacer) )||' '||LOWER(l_object_name)||'.'||LOWER( column_name )||'%TYPE'||DECODE(nullable,'Y',' DEFAULT NULL',null)|| DECODE(lead(column_name) over  ( ORDER BY  column_id),NULL,' );',',') line
                 FROM    cols
                 WHERE   table_name = UPPER(l_object_name)
                 AND column_name NOT LIKE '%CREATED_BY%'
                 AND column_name NOT LIKE '%CREATED_DATE%'
                 AND column_name NOT LIKE '%UPDATE_BY%'
                 AND column_name NOT LIKE '%UPDATE_DATE%'
                 ORDER BY column_id ) LOOP

         l_final_sql := l_final_sql||chr(13)||i.line;
 

      END LOOP;

 

      FOR i IN ( SELECT DECODE( lag(column_name) over  ( ORDER BY column_id),NULL, CHR(13)||'   PROCEDURE update_row( ','                         ') ||'p_'||RPAD(LOWER( column_name ),8 * l_tabstops,CHR(l_spacer))||' '||LOWER(l_object_name)||'.'||LOWER( column_name )||'%TYPE'||DECODE(nullable,'Y',' DEFAULT NULL',null)|| DECODE(lead(column_name) over  ( ORDER BY  column_id),NULL,' );',',') line
                 FROM    cols
                 WHERE   table_name = UPPER(l_object_name)
                 AND column_name NOT LIKE '%CREATED_BY'
                 AND column_name NOT LIKE '%CREATED_DATE'
                 AND column_name NOT LIKE '%LAST_UPDATE_BY' ) LOOP

         l_final_sql := l_final_sql||chr(13)||i.line;
 

      END LOOP;

 

      FOR i IN ( SELECT DECODE( lag(column_name) over  ( ORDER BY column_id),NULL, CHR(13)||'   PROCEDURE delete_row( ','                         ') ||'p_'||RPAD(LOWER( column_name ),8 * l_tabstops,CHR(l_spacer))||' '||LOWER(l_object_name)||'.'||LOWER( column_name )||'%TYPE'||DECODE(nullable,'Y',' DEFAULT NULL',null)|| DECODE(lead(column_name) over  ( ORDER BY  column_id),NULL,' );',',') line
                 FROM    cols
                 WHERE   table_name = UPPER(l_object_name)
                 AND column_name  IN ( SELECT column_name
                                       FROM user_cons_columns ucc,
                                            user_constraints uc
                                       WHERE uc.owner    = ucc.owner
                                       AND uc.table_name = ucc.table_name
                                       AND uc.constraint_name = ucc.constraint_name
                                       AND uc.table_name = UPPER(p_table_name)
                                       AND constraint_type = 'P' )
                 ORDER BY column_id ) LOOP

 

          debug( i.line);

 

         l_final_sql := l_final_sql||chr(13)||i.line;

 
      END LOOP;

 

      l_final_sql := l_final_sql||chr(13)||chr(13)||'END '||LOWER(l_package_name)||';'||CHR(13)||'/'||CHR(13)||
                                  chr(13)||chr(13)||'CREATE OR REPLACE PACKAGE BODY '||LOWER(l_package_name)||' AS'||CHR(13)||CHR(13);

 

 

   /* --------------------------------------------------------------------------------------

      Retrieve the LINES for the INSERT PROCEDURE for PACKAGE BODY

      -------------------------------------------------------------------------------------- */

 

      FOR i IN ( SELECT DECODE( lag(column_name) over  ( ORDER BY column_id),NULL, '   PROCEDURE insert_row( ','                         ') ||'p_'||RPAD(LOWER( column_name ),8 * l_tabstops,CHR(l_spacer) )||' '||LOWER(l_object_name)||'.'||LOWER( column_name )||'%TYPE'||DECODE(nullable,'Y',' DEFAULT NULL',null)|| DECODE(lead(column_name) over  ( ORDER BY  column_id),NULL,' ) IS'||CHR(13),',') line
                 FROM    cols
                 WHERE   table_name = UPPER(l_object_name)
                 AND column_name NOT LIKE '%CREATED_BY%'
                 AND column_name NOT LIKE '%CREATED_DATE%'
                 AND column_name NOT LIKE '%UPDATE_BY%'
                 AND column_name NOT LIKE '%UPDATE_DATE%'
                 ORDER BY column_id ) LOOP

         l_final_sql := l_final_sql||chr(13)||i.line;
 

      END LOOP;

 

      l_final_sql:= l_final_sql||CHR(13)||'   BEGIN'||chr(13);

 
      FOR i IN ( SELECT DECODE( lag(column_name) over  ( ORDER BY column_id),NULL, '   INSERT '||CHR(13)||'   INTO '||LOWER(l_object_name)||chr(13)||'      ('||chr(13) )||'        '  ||LOWER( column_name )|| DECODE(lead(column_name) over  ( ORDER BY  column_id),NULL,' )',',') line
                 FROM    cols
                 WHERE   table_name = UPPER(l_object_name)
                 AND column_name NOT LIKE '%CREATED_BY'
                 AND column_name NOT LIKE '%CREATED_DATE'
                 AND column_name NOT LIKE '%LAST_UPDATE_BY'
                 AND column_name NOT LIKE '%UPDATE_DATE%'
                 ORDER BY column_id ) LOOP

 

         l_final_sql := l_final_sql||chr(13)||i.line;

      END LOOP;

 

 

      FOR i IN ( SELECT  DECODE( lag(column_name) over ( ORDER BY column_id), NULL,'   VALUES '||CHR(13)||'      ( '||CHR(13) )||
                         '       p_'||LOWER( column_name )||DECODE(lead(column_name) over  ( ORDER BY  column_id),NULL,');'||CHR(13)||CHR(13)||' END insert_row;',',') line
                 FROM    cols
                 WHERE   table_name = UPPER(l_object_name)
                 AND column_name NOT LIKE '%CREATED_BY'
                 AND column_name NOT LIKE '%CREATED_DATE'
                 AND column_name NOT LIKE '%LAST_UPDATE_BY'
                 AND column_name NOT LIKE '%UPDATE_DATE%'
                 ORDER BY column_id ) LOOP

         l_final_sql := l_final_sql||chr(13)||i.line; 

      END LOOP;

 

      FOR i IN ( SELECT DECODE( lag(column_name) over  ( ORDER BY column_id),NULL, CHR(13)||'   PROCEDURE update_row( ','                         ') ||'p_'||RPAD(LOWER( column_name ),8 * l_tabstops,CHR(l_spacer))||' '||LOWER(l_object_name)||'.'||LOWER( column_name )||'%TYPE'||DECODE(nullable,'Y',' DEFAULT NULL',null)|| DECODE(lead(column_name) over  ( ORDER BY  column_id),NULL,' ) IS'||CHR(13)||CHR(13),',') line
                 FROM    cols
                 WHERE   table_name = UPPER(l_object_name)
                 AND column_name NOT LIKE '%CREATED_BY'
                 AND column_name NOT LIKE '%CREATED_DATE'
                 AND column_name NOT LIKE '%LAST_UPDATE_BY' ) LOOP

         l_final_sql := l_final_sql||chr(13)||i.line;


      END LOOP;
 

      FOR i IN ( SELECT '   l_'||RPAD(LOWER( column_name ),30,' ')||' '||LOWER(l_object_name)||'.'||LOWER( column_name )||'%TYPE;'||CHR(13)||CHR(13)||
                        '   recordChangedError   EXCEPTION;'||CHR(13) line
                 FROM    cols
                 WHERE   table_name = UPPER(l_object_name)
                 AND (column_name LIKE '%UPDATE_DATE%'
                      OR column_name LIKE '%UPDATED_DATE%') ) LOOP
 
         l_final_sql := l_final_sql||chr(13)||i.line||CHR(13)||'   BEGIN'||CHR(13);
     
      END LOOP;


      FOR i IN ( SELECT line
                 FROM (
                 SELECT '      '||DECODE( lag(column_name) over  ( ORDER BY column_id),NULL, 'SELECT','      ') ||' '||LOWER( column_name )||DECODE( lead(column_name) over  ( ORDER BY column_id),NULL, NULL,',') line,
                    1 dataset,
                     column_id
                 FROM    cols
                 WHERE   table_name = UPPER(l_object_name)
                 AND (column_name LIKE '%UPDATE_DATE%'
                      OR column_name LIKE '%UPDATED_DATE%')
                 UNION ALL
                 SELECT '      '||DECODE( lag(column_name) over  ( ORDER BY column_id),NULL, 'INTO','    ') ||' l_'||LOWER( column_name )||DECODE( lead(column_name) over  ( ORDER BY column_id),NULL, NULL,','),
                    2 dataset,
                     column_id
                 FROM    cols
                 WHERE   table_name = UPPER(l_object_name)
                 AND (column_name LIKE '%UPDATE_DATE%'
                      OR column_name LIKE '%UPDATED_DATE%')
                  ) ORDER BY dataset,column_id

               ) LOOP

         l_final_sql := l_final_sql||chr(13)||i.line;


      END LOOP; 

      l_final_sql := l_final_sql ||CHR(13)||'      FROM '||LOWER(l_object_name);

      FOR i IN ( SELECT '      '||DECODE( lag(column_name) over  ( ORDER BY position),NULL, 'WHERE','AND  ')||' '||RPAD(LOWER(column_name),25,' ')||'= p_'||LOWER(column_name)||DECODE(lead(column_name) over  ( ORDER BY  position ),NULL,CHR(13)||'      FOR UPDATE NOWAIT;'||CHR(13) ,NULL) line
                 FROM user_cons_columns ucc,
                      user_constraints uc
                 WHERE uc.owner = ucc.owner
                 AND uc.table_name = ucc.table_name
                 AND uc.constraint_name = ucc.constraint_name
                 AND uc.table_name = UPPER(p_table_name)
                 AND column_name NOT LIKE '%ORG_ID'
                 AND constraint_type = 'P' ) LOOP

         l_final_sql := l_final_sql||chr(13)||i.line;

      END LOOP;          

 
      FOR i IN ( SELECT '      '||DECODE( lag(column_name) over  ( ORDER BY column_id),NULL, 'IF','  ')||' l_'|| LOWER( column_name )|| ' != p_'|| LOWER( column_name )|| DECODE( lead(column_name) over  ( ORDER BY column_id),NULL, ' THEN' ||CHR(13)||'         RAISE recordChangedError;'||CHR(13)||'      END IF;'||CHR(13),' OR') line
                 FROM    cols
                 WHERE   table_name = UPPER(l_object_name)
                 AND (column_name LIKE '%UPDATE_DATE%'
                      OR column_name LIKE '%UPDATED_DATE%')
                 ORDER BY column_id ) LOOP

         l_final_sql := l_final_sql||chr(13)||i.line;     

      END LOOP;          
 
      FOR i IN ( SELECT DECODE( lag(column_name) over  ( ORDER BY column_id),NULL, '      UPDATE '||LOWER(l_object_name))||'      '||DECODE( lag(column_name) over  ( ORDER BY column_id),NULL, CHR(13)||'      SET','   ')||' '||RPAD(LOWER( column_name ),8 * l_tabstops,CHR(l_spacer) ) || ' = p_'||LOWER( column_name )||DECODE( lead(column_name) over  ( ORDER BY column_id),NULL,NULL,',') line
                 FROM    cols
                 WHERE   table_name = UPPER(l_object_name)
                 AND column_name  NOT IN ( SELECT column_name
                                      FROM user_cons_columns ucc,
                                           user_constraints uc
                                      WHERE uc.owner = ucc.owner
                                      AND uc.table_name = ucc.table_name
                                      AND uc.constraint_name = ucc.constraint_name
                                      AND uc.table_name = UPPER(p_table_name)
                                      AND constraint_type = 'P'
                                       )
                 AND column_name NOT LIKE '%CREATED_BY'
                 AND column_name NOT LIKE '%CREATED_DATE'
                 AND column_name NOT LIKE '%UPDATE_BY'
                 AND column_name NOT LIKE '%UPDATE_DATE'
                 ORDER BY column_id ) LOOP

         l_final_sql := l_final_sql||chr(13)||i.line;     

      END LOOP;          

      FOR i IN ( SELECT '      '||DECODE( lag(column_name) over  ( ORDER BY position),NULL, 'WHERE','AND  ')||' '||RPAD(LOWER(column_name),25,' ')||'= p_'||LOWER(column_name)||DECODE(lead(column_name) over  ( ORDER BY  position ),NULL,';',NULL) line
                 FROM user_cons_columns ucc,
                      user_constraints uc
                 WHERE uc.owner = ucc.owner
                 AND uc.table_name = ucc.table_name
                 AND uc.constraint_name = ucc.constraint_name
                 AND uc.table_name = UPPER(p_table_name)
                 AND constraint_type = 'P'
                 AND column_name NOT LIKE '%ORG_ID'
                 ORDER BY position ) LOOP
 
         l_final_sql := l_final_sql||chr(13)||i.line;
     
      END LOOP;          

      l_final_sql := l_final_sql||CHR(13)||CHR(13)||'   EXCEPTION'||CHR(13)||
                                                    '      WHEN recordChangedError THEN '||CHR(13)||
                                                    '         RAISE_APPLICATION_ERROR( -20001, ''Another User has already amended this record'');'||CHR(13)||
                                                    '      WHEN NO_DATA_FOUND THEN '||CHR(13)||
                                                    '         RAISE_APPLICATION_ERROR( -20002, ''Another User has Deleted this record'');'||CHR(13)||
                                                    '   END update_row;'||CHR(13)||CHR(13);

 

      FOR i IN ( SELECT DECODE( lag(column_name) over  ( ORDER BY column_id),NULL, CHR(13)||'   PROCEDURE delete_row( ','                         ') ||'p_'||RPAD(LOWER( column_name ),8 * l_tabstops,CHR(l_spacer))||' '||LOWER(l_object_name)||'.'||LOWER( column_name )||'%TYPE'||DECODE(nullable,'Y',' DEFAULT NULL',null)|| DECODE(lead(column_name) over  ( ORDER BY  column_id),NULL,' ) IS'||CHR(13)||CHR(13)||'   BEGIN'||CHR(13),',') line
                 FROM    cols
                 WHERE   table_name = UPPER(l_object_name)
                 AND column_name  IN ( SELECT column_name
                                       FROM user_cons_columns ucc,
                                            user_constraints uc
                                       WHERE uc.owner    = ucc.owner
                                       AND uc.table_name = ucc.table_name
                                       AND uc.constraint_name = ucc.constraint_name
                                       AND uc.table_name = UPPER(p_table_name)
                                       AND constraint_type = 'P' )
                 ORDER BY column_id ) LOOP

         l_final_sql := l_final_sql||chr(13)||i.line;

      END LOOP;

 

     

      FOR i IN (  SELECT '      '||DECODE( lag(column_name) over  ( ORDER BY position),NULL, 'DELETE '||CHR(13)||'      FROM '||LOWER( l_object_name )||CHR(13)||'      WHERE','      AND  ')||' '||RPAD(LOWER(column_name),25,' ')||'= p_'||LOWER(column_name)||DECODE(lead(column_name) over  ( ORDER BY  position ),NULL,';'||CHR(13),NULL) line
                  FROM user_cons_columns ucc,
                       user_constraints uc
                  WHERE uc.owner = ucc.owner
                  AND uc.table_name = ucc.table_name
                  AND uc.constraint_name = ucc.constraint_name
                  AND uc.table_name = UPPER(p_table_name)
                  AND constraint_type = 'P'
                  AND column_name NOT LIKE '%ORG_ID'
                  ORDER BY position ) LOOP
 
         l_final_sql := l_final_sql||chr(13)||i.line;

      END LOOP;

 

      l_final_sql := l_final_sql||CHR(13)||CHR(13)||'      IF SQL%NOTFOUND THEN '||CHR(13)||
                                                    '         RAISE NO_DATA_FOUND; '||CHR(13)||
                                                    '      END IF; '||CHR(13)||CHR(13)||
                                                    '   EXCEPTION'||CHR(13)||
                                                    '      WHEN NO_DATA_FOUND THEN '||CHR(13)||
                                                    '         RAISE_APPLICATION_ERROR( -20002, ''Another User has Deleted this record'');'||CHR(13)||
                                                    '   END delete_row;'||CHR(13)||CHR(13)||
                                                    'END '||LOWER(l_package_name)||';'||CHR(13)||CHR(13)||'/'||CHR(13);

 

      INSERT
      INTO tapi_gen
      VALUES( p_table_name,l_final_sql);
 
      COMMIT;

 

  END tapi;

 

  PROCEDURE create_view( p_view_name VARCHAR2 ) IS

 

  BEGIN

  FOR i IN (

         SELECT 'CREATE OR REPLACE VIEW '||table_name||'_VW AS SELECT ' ||LTRIM(MAX(sys_connect_by_path(column_name,', ') )
         KEEP (dense_rank last ORDER BY curr),', ')|| (
                                                       SELECT MAX(' FROM '||uc.table_name||' WHERE '||column_name||' = application_utils.get_organisation ')
                                                       FROM user_cons_columns ucc,
                                                            user_constraints uc
                                                       WHERE uc.table_name = ucc.table_name
                                                       AND uc.constraint_name = ucc.constraint_name
                                                       AND uc.table_name = a.table_name
                                                       AND ucc.column_name LIKE '%ORG_ID' AND constraint_type IN ('P','U' )
   )  AS sql_text
              FROM
              (
                  SELECT table_name,
                             column_name,
                             row_number() over (PARTITION BY table_name ORDER BY column_id) AS curr,
                             row_number() over (PARTITION BY table_name ORDER BY column_id) -1 AS prev
                      FROM   user_tab_columns
                  WHERE table_name = upper(p_view_name)
              ) a
              GROUP BY table_name
              CONNECT BY prev = PRIOR curr AND table_name = PRIOR table_name
              START WITH curr = 1
  ) LOOP

     BEGIN

        EXECUTE IMMEDIATE i.sql_text;

     EXCEPTION

        WHEN OTHERS THEN 

           dbms_output.put_line(i.sql_text);

           RAISE;

     END;  

  END LOOP;

  END create_view;

 

END generate_pkg;

/