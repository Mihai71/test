Feature: Setup Upload Initial

  Scenario:
    Given url 'http://127.0.0.1:8000'
    And path '/upload'
    And multipart file file = { read: 'test_data.csv', filename: 'test_data.csv', contentType: 'text/csv' }
    When method POST
    Then status 200
    * def fId = response.file_id[0]