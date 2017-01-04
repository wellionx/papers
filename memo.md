#git init 在目录中创建新的 Git 仓库
git init #目录变仓库

#首次连接远端
git remote rm origin
git remote add origin git@github.com:wellionx/papers.git


git push origin master

#本地删除文件，使远端与本地同步
git add *
git commit -a -m "update"
git push origin master

#添加本地改变的文件
git add .
git commit -m "zhushi" #message
git push origin master

#上传不成功
git branch -a #查看所有的分支

在本地删除一个分支： git branch -d Branch1
在github远程端删除一个分支： git push origin :Branch1 
可以通过命令 Git remote show [remote-name] 查看某个远程仓库的详细信息