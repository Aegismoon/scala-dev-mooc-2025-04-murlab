package ru.otus.module4.homework.dao.repository

import zio.{ULayer, ZIO, ZLayer}
import io.getquill.context.ZioJdbc._
import ru.otus.module4.homework.dao.entity._
import ru.otus.module4.phoneBook.db

import java.sql.SQLException
import javax.sql.DataSource

trait UserRepository{
    def findUser(userId: UserId): QIO[Option[User]]
    def createUser(user: User): QIO[User]
    def createUsers(users: List[User]): QIO[List[User]]
    def updateUser(user: User): QIO[Unit]
    def deleteUser(user: User): QIO[Unit]
    def findByLastName(lastName: String): QIO[List[User]]
    def list(): QIO[List[User]]
    def userRoles(userId: UserId): QIO[List[Role]]
    def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]
    def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
}


class UserRepositoryImpl extends UserRepository {
    val dc = db.Ctx
    import dc._

    val userSchema = quote{
        querySchema[User]("""User""")
    }

    val roleSchema = quote{
        querySchema[Role]("""Role""")
    }

    val userRoleSchema= quote{
        querySchema[UserToRole]("""UserToRole""")
    }

    override def findUser(userId: UserId): QIO[Option[User]] =
        dc.run(userSchema.filter(_.id == lift(userId.id)))

    override def createUser(user: User): QIO[User] =
        dc.run(userSchema.insertValue(lift(user)))

    override def createUsers(users: List[User]): QIO[List[User]] =
        dc.run(liftQuery(users).foreach(user => userSchema.insertValue(user)))

    override def updateUser(user: User): QIO[Unit] =
        dc.run(userSchema.updateValue(lift(user)))

    override def deleteUser(user: User): QIO[Unit] =
        dc.run(userSchema.filter(_.id == lift(user.id)).delete)

    override def findByLastName(lastName: String): QIO[List[User]] =
        dc.run(userSchema.filter(_.lastName == lift(lastName)))

    override def list(): QIO[List[User]] =
        dc.run(userSchema)

    override def userRoles(userId: UserId): QIO[List[Role]] =
        dc.run( for{
            userRoleFact <- userRoleSchema.filter(_.userId == lift(userId.id))
            roleDim <- roleSchema.filter(_.code == userRoleFact.roleId)
        } yield roleDim)

    override def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit] =
        dc.run(userRoleSchema.insertValue(lift(UserToRole(roleCode.code,userId.id))))

    override def listUsersWithRole(roleCode: RoleCode): QIO[List[User]] =
        dc.run(      for{
            userRoleFact <- userRoleSchema.filter(_.roleId == lift(roleCode.code))
            userDim <- userSchema.join(_.id == userRoleFact.userId)
        } yield userDim)

    override def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]] =
        dc.run(roleSchema.filter(_.code == lift(roleCode.code)))
}

object UserRepository{

    val layer: ULayer[UserRepository] = ZLayer.succeed(new UserRepositoryImpl)
}