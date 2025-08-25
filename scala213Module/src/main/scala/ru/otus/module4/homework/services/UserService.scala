package ru.otus.module4.homework.services

import io.getquill.context.ZioJdbc.QIO
import ru.otus.module4.homework.dao.entity.{Role, RoleCode, User}
import ru.otus.module4.homework.dao.repository.UserRepository
import ru.otus.module4.phoneBook.db
import zio.{ZIO, ZLayer}

trait UserService{
    def listUsers(): QIO[List[User]]
    def listUsersDTO(): QIO[List[UserDTO]]
    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]]
}
class Impl(userRepo: UserRepository) extends UserService {
    val dc = db.Ctx
    import dc._
    def listUsers(): QIO[List[User]] =
        userRepo.list()


    def listUsersDTO(): QIO[List[UserDTO]] =
        for{
         users <- userRepo.list()
         userDTOs <-  ZIO.foreach(users) { u=> userRepo.userRoles(u.typedId).map(rs => UserDTO(u, rs.toSet))}
         } yield userDTOs

    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO] =
            for {
                _     <- userRepo.createUser(user)
                _     <- userRepo.insertRoleToUser(roleCode = roleCode, userId = user.typedId)
                roles <- userRepo.userRoles(user.typedId)
            } yield UserDTO(user, roles.toSet)


    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]] =
        for {
            users <- userRepo.listUsersWithRole(roleCode)
            dtos  <- ZIO.foreach(users) { u =>
                userRepo.userRoles(u.typedId)
                  .map(rs => UserDTO(u, rs.toSet))
            }
        } yield dtos

}
object UserService{

    val layer: ZLayer[UserRepository, Nothing, UserService] =
        ZLayer(
            for {
                userServiceRepo <- ZIO.service[UserRepository]
            } yield new Impl(userServiceRepo)
        )
}

case class UserDTO(user: User, roles: Set[Role])