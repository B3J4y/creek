import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.WriteAbortedException;
import java.util.List;

import jxl.Sheet;
import jxl.Workbook;
import jxl.read.biff.BiffException;
import jxl.write.Label;
import jxl.write.WritableSheet;
import jxl.write.WritableWorkbook;
import jxl.write.WriteException;
import jxl.write.biff.RowsExceededException;


public class Dunno {
	String str1 = "5,10),w),p(f(6,10),b),p(f(6,8),w),p(f(6,7),w),p(f(7,8),b),p(f(3,1),w),p(f(2,1),b),p(f(7,7),b),p(f(8,6),b),p(f(7,6),b),p(f(8,7),w),p(f(8,10),w),p(f(8,9),w),p(f(9,10),b),p(f(4,9),w),p(f(3,9),b),p(f(4,1),w),p(f(5,1),w),p(f(6,2),w),p(f(6,1),w),p(f(7,1),b),p(f(7,2),b),p(f(10,5),b),p(f(9,5),b),p(f(10,4),b),p(f(8,5),w),p(f(9,4),w),p(f(8,4),w),p(f(9,7),w),p(f(9,6),w),p(f(10,6),b),p(f(10,7),b),p(f(8,8),w),p(f(9,8),w),p(f(10,8),b),p(f(10,9),b),p(f(9,3),w),p(f(10,3),w),p(f(10,2),w),p(f(10,1),b),p(f(7,10),w),p(f(7,9),w),p(f(6,9),w),p(f(5,9),w),p(f(5,8),w),p(f(5,7),w),p(f(4,7),w),p(f(3,7),w),p(f(3,8),b),p(f(4,8),b),p(f(7,4),b),p(f(8,3),b),p(f(7,3),b),p(f(9,9),b),p(f(3,4),b),p(f(2,4),b),p(f(3,3),b),p(f(2,3),w),p(f(3,2),w),p(f(2,2),w),p(f(1,3),w),p(f(1,4),w),p(f(1,5),w),p(f(3,6),w),p(f(2,5),w),p(f(3,5),w),p(f(4,4),w),p(f(2,8),w),p(f(1,8),w),p(f(4,10),w),p(f(3,10),w),p(f(2,10),b),p(f(1,10),b),p(f(2,9),b),p(f(1,9),w),p(f(6,3),w),p(f(6,6),w),p(f(7,5),b),p(f(5,6),b),p(f(4,6),b),p(f(4,5),b),p(f(10,10),w),p(f(9,2),b),p(f(9,1),b),p(f(8,2),b),p(f(8,1),b),p(f(6,5),w),p(f(6,4),w),p(f(5,5),w),p(f(5,4),w),p(f(5,3),b),p(f(5,2),b),p(f(4,3),b),p(f(4,2),b),p(f(2,7),b),p(f(2,6),b),p(f(1,7),b),p(f(1,6),b),p(f(1,2),b),p(f(1,1),b";
	
	public String[][] sortAll(String[] strs){
		String[][] strArr = new String[10][10];
		for(String iStr : strs){
			if(iStr.length()>0){
				int numb1 = Integer.valueOf(iStr.trim().split("\\),")[0].split(",")[0]) - 1;
				int numb2 = Integer.valueOf(iStr.trim().split("\\),")[0].split(",")[1]) - 1;
				if(iStr.split("\\),")[1].contains("b")){
					strArr[numb1][numb2] = "x";
				} else{
					if(iStr.split("\\),")[1].contains("t")){
						strArr[numb1][numb2] = "t";
					} else{
						if(iStr.split("\\),")[1].contains("w(")){
							strArr[numb1][numb2] = iStr.split("\\),")[1];
						} else{
							strArr[numb1][numb2] = " ";
							
						}
					}
				}
			}
		}
		return strArr;
	}
	
	public void readOutTxtFile(String path) throws IOException, InterruptedException{
		File file = new File(path);
		BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file)));
		String line ="";
		String txt;
		int linecount = 0;
		for(line = br.readLine(); line != null;line = br.readLine()){

			//System.out.println(line);
			String[] strT = line.split("\\[p\\(f\\(");
			String[][] data = sortAll(line.split("\\[p\\(f\\(")[1].split("\\)\\]")[0].split("\\),p\\(f\\("));
			for(int i = 0; i < 10; i++){
				String str = "";
				for(int j = 0; j < 10; j++){
					if(data[j][i] !=null){
						str += data[j][i];
					}
				}
				if(str.length() > 0){
					System.out.println(str);
				}
			}
			linecount++;
			System.out.println(linecount);
			
		};
		br.close();
	}
	
	public String[] readOutExcel(String path) throws IOException{
        File inputWorkbook = new File(path);
        Workbook w;
        String[] data = new String[1];
        try 
        {
        	
            w = Workbook.getWorkbook(inputWorkbook);
            // Get the first sheet
            Sheet sheet = w.getSheet(0);
            // Loop over first 10 column and lines
            data = new String[sheet.getRows()];
            for (int j = 0; j < sheet.getRows(); j++) 
            {
                data[j] = sheet.getCell(0,j).getContents();
            }
        } catch (BiffException e) 
        {
            e.printStackTrace();
        }
        return data;
	}
	public void writeExcel(String path, String[][] data) throws IOException, RowsExceededException, WriteException{
		File inputWorkbook = new File(path);
		
		WritableWorkbook writableWorkbook = Workbook.createWorkbook(inputWorkbook);
 
		WritableSheet writableSheet = writableWorkbook.createSheet("Sheet1", 0);

		// Get the first sheet
		//Sheet sheet = w.getSheet(0);
		// Loop over first 10 column and lines
		//data = new String[sheet.getRows()];
		for (int j = 0; j < data.length; j++) 
		{
			for (int i = 0; i < data[j].length; i++) {
				Label label = new Label(j, i, data[j][i]);
				writableSheet.addCell(label);
			}
		}
		writableWorkbook.write();
        writableWorkbook.close();
	}

	public static void main(String[] args) throws IOException, RowsExceededException, WriteException, InterruptedException {
		Dunno d = new Dunno();
//		for(String str : d.readOutExcel("/Users/jbe/Documents/Ergebn.xls")){
//			System.out.println(str.trim().split("\\),")[0].split("p\\(f\\(")[1]);
//			System.out.println(str.trim().split("\\),")[1]);
//		}
//		
//		String[][] data = d.sortAll(d.readOutExcel("/creek/creek/debug.txt"));
//		d.writeExcel("/Users/jbe/Documents/Ergebnis.xls", data);
//		for(int i = 0; i < 10; i++){
//			String str = "";
//			for(int j = 0; j < 10; j++){
//				str += data[j][i];
//			}
//			System.out.println(str);
//		}
		
		d.readOutTxtFile("/Users/jbe/creek/creek/test.txt");
	}
}
